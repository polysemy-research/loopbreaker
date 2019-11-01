{-# LANGUAGE TemplateHaskell #-}

module Loopbreaker.InlineRecCalls (action) where

import           Control.Arrow hiding ((<+>))
import           Data.Generics
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S

import Bag
import ErrUtils
import GhcPlugins hiding ((<>), debugTraceMsg)
import HsSyn
import MonadUtils


------------------------------------------------------------------------------
type MonadInline m = ((MonadUnique m, MonadIO m, HasDynFlags m) :: Constraint)

------------------------------------------------------------------------------
-- | Forces compiler to inline functions by creating loopbreaker with NOINLINE
-- pragma, changing recursive calls to use it and by adding INLINE pragma to
-- the original function.
action :: MonadInline m
       => [CommandLineOption] -> HsGroup GhcRn -> m (HsGroup GhcRn)
action opts group@HsGroup{ hs_valds } = do
  let shouldDisable = "disable" `elem` opts

  dyn_flags <- getDynFlags

  if not shouldDisable && optLevel dyn_flags > 0
    then do
      liftIO $ showPass dyn_flags "Break loops"
      valds' <- inlineRecCalls hs_valds
      pure group{ hs_valds = valds' }
    else
      pure group

action _ _ = error "Loopbreaker.InlineRecCalls.action: expected renamed group"

------------------------------------------------------------------------------
inlineRecCalls :: MonadInline m => HsValBinds GhcRn -> m (HsValBinds GhcRn)
inlineRecCalls (XValBindsLR (NValBinds binds sigs)) = do
  let (types, inlined) = typesFromSigs &&& inlinedFromSigs $ unLoc <$> sigs

  (binds', extra_sigs) <- second concat . unzip
                      <$> traverse (inlineRecCall types inlined) binds

  pure $ XValBindsLR $ NValBinds binds' $ sigs ++ extra_sigs

-- TODO: should we throw an error here instead?
inlineRecCalls val_binds = pure val_binds

------------------------------------------------------------------------------
typesFromSigs :: Ord (IdP p) => [Sig p] -> Map (IdP p) (LHsSigWcType p)
typesFromSigs = M.fromList . concatMap sigToTups where
  sigToTups (TypeSig _ names type_) = (,type_) . unLoc <$> names
  sigToTups _                       = []

------------------------------------------------------------------------------
inlinedFromSigs :: Ord (IdP p) => [Sig p] -> Set (IdP p)
inlinedFromSigs = S.fromList . catMaybes . map nameIfInlineSig where
  nameIfInlineSig (InlineSig _ (L _ name) pragma)
    | isInlinePragma pragma = Just name
  nameIfInlineSig _ = Nothing

------------------------------------------------------------------------------
-- | Inserts loopbreaker to recursive binding group of single binding and
-- emits necessary signatures.
inlineRecCall
  :: MonadInline m
  => Map Name (LHsSigWcType GhcRn)  -- ^ types of bindings
  -> Set Name                       -- ^ Loopbreaker annotations
  -> (RecFlag, LHsBinds GhcRn)      -- ^ binding being inlined
  -> m ((RecFlag, LHsBinds GhcRn), [LSig GhcRn])
inlineRecCall types inlined (Recursive, binds)
  | (bagToList -> [L fun_loc fun_bind])           <- binds
  , FunBind{ fun_id = L _ fun_name, fun_matches } <- fun_bind
  , S.member fun_name inlined
  = do
  dyn_flags <- getDynFlags
  liftIO $ debugTraceMsg dyn_flags 2 $ text "Loopbreaker:" <+> ppr fun_name

  let fun_type = M.lookup fun_name types

  (loopb_name, loopb_decl) <- loopbreaker fun_name fun_type

  let m_loopb_sig = loopbreakerSig loopb_name <$> fun_type

  fun_matches' <- everywhereM ( fmap (replaceVarNamesT fun_name loopb_name)
                              . inlineLocalRecCallsM
                              ) fun_matches


  pure
    ( ( Recursive
      , listToBag
          [ L fun_loc fun_bind{ fun_matches = fun_matches' }
          , loopb_decl
          ]
      )
      -- If the original function didn't have type signature specified, we
      -- shouldn't have to have either
    , inlineSig noInlinePragma loopb_name : maybeToList m_loopb_sig
    )
-- We ignore mutually recursive and other bindings
inlineRecCall _ _ binds = pure (binds, [])

------------------------------------------------------------------------------
-- | Creates loopbreaker and it's name from name of the original function.
loopbreaker :: MonadUnique m
            => Name -> Maybe (LHsSigWcType GhcRn) -> m (Name, LHsBind GhcRn)
loopbreaker fun_name fun_type =
  (id &&& loopbreakerDecl fun_name fun_type) <$> loopbreakerName fun_name

------------------------------------------------------------------------------
loopbreakerName :: MonadUnique m => Name -> m Name
loopbreakerName (occName -> occNameFS -> orig_fs) =
  flip mkSystemVarName (orig_fs <> "__Loopbreaker") <$> getUniqueM

------------------------------------------------------------------------------
loopbreakerDecl :: Name -> Maybe (LHsSigWcType GhcRn) -> Name -> LHsBind GhcRn
loopbreakerDecl fun_name fun_type loopb_name =
  noLoc $ mkTopFunBind Generated (noLoc loopb_name)
    [ mkSimpleMatch (mkPrefixFunRhs $ noLoc loopb_name) [] $
        maybe id (fmap noLoc . ExprWithTySig) fun_type $ nlHsVar fun_name
    ]

------------------------------------------------------------------------------
-- | Creates loopbreaker type signature from type of original function.
loopbreakerSig :: Name -> LHsSigWcType GhcRn -> LSig GhcRn
loopbreakerSig loopb_name fun_type =
  noLoc $ TypeSig NoExt [noLoc loopb_name] fun_type

------------------------------------------------------------------------------
inlineSig :: (XInlineSig p ~ NoExt) => InlinePragma -> IdP p -> LSig p
inlineSig how name = noLoc $ InlineSig NoExt (noLoc name) how

------------------------------------------------------------------------------
-- | Contrary to 'neverInlinePragma', this has behaviour of @NOINLINE@ pragma.
noInlinePragma :: InlinePragma
noInlinePragma = defaultInlinePragma
  { inl_inline = NoInline
  , inl_act    = NeverActive
  }

------------------------------------------------------------------------------
-- | Transformation that applies loopbreakers to local bindings.
inlineLocalRecCallsM :: (MonadInline m, Typeable a) => a -> m a
inlineLocalRecCallsM = mkM $ \case
  (HsValBinds NoExt binds :: HsLocalBinds GhcRn)
    -> HsValBinds NoExt <$> inlineRecCalls binds
  e -> pure e

------------------------------------------------------------------------------
-- | Transformation that replaces every name in variable expression.
replaceVarNamesT :: Typeable a => Name -> Name -> a -> a
replaceVarNamesT from to = mkT $ \case
  HsVar NoExt (L loc name) :: HsExpr GhcRn
    | name == from -> HsVar NoExt $ L loc to
  e -> e
