{-# LANGUAGE TemplateHaskell #-}

module Loopbreaker.InlineRecCalls (action) where

import           Control.Arrow hiding ((<+>))
import           Data.Bool
import           Data.Generics
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Language.Haskell.TH.Syntax as TH

import Bag
import Convert
import ErrUtils
import GhcPlugins hiding ((<>), debugTraceMsg)
import HsSyn
import MonadUtils

import Loopbreaker


------------------------------------------------------------------------------
type MonadInline m = ((MonadUnique m, MonadIO m, HasDynFlags m) :: Constraint)

------------------------------------------------------------------------------
-- | Forces compiler to inline functions by creating loopbreaker with NOINLINE
-- pragma, changing recursive calls to use it and by adding INLINE pragma to
-- the original function.
action :: MonadInline m
       => [CommandLineOption] -> HsGroup GhcRn -> m (HsGroup GhcRn)
action opts group = do
  -- Last option takes precedence, on by default
  let isDefaultOn = fromMaybe True $ getLast
                  $ flip foldMap opts $ Last . \case
                      "default_on"  -> Just True
                      "default_off" -> Just False
                      _             -> Nothing

  dyn_flags <- getDynFlags

  if optLevel dyn_flags > 0
    then do
      liftIO $ showPass dyn_flags "Add loopbreakers"
      inlineRecCalls isDefaultOn group
    else
      pure group

------------------------------------------------------------------------------
inlineRecCalls :: MonadInline m => Bool -> HsGroup GhcRn -> m (HsGroup GhcRn)
inlineRecCalls isDefaultOn group
  | HsGroup { hs_valds = XValBindsLR (NValBinds binds sigs)
            , hs_annds = map unLoc -> anns
            } <- group
  = do
  let types      = typesMap sigs
      loopb_anns = loopbreakerAnnsMap anns

  (binds', extra_sigs) <- second concat . unzip <$>
    traverse (inlineRecCall isDefaultOn types loopb_anns) binds

  pure group{ hs_valds = XValBindsLR $ NValBinds binds' $ sigs ++ extra_sigs }

inlineRecCalls _ _ = error "inlineRecCalls: expected renamed group"

------------------------------------------------------------------------------
typesMap :: Ord (IdP p) => [LSig p] -> Map (IdP p) (LHsSigWcType p)
typesMap = M.fromList . concatMap sigToTups where
  sigToTups (L _ (TypeSig _ names type_)) = (,type_) . unLoc <$> names
  sigToTups _                             = []

------------------------------------------------------------------------------
loopbreakerAnnsMap :: [AnnDecl GhcRn] -> Map Name LoopbreakerAnn
loopbreakerAnnsMap = M.fromList . catMaybes . map annToTup where
  annToTup (HsAnnotation _ _ (ValueAnnProvenance (L _ name)) (L _ expr))
    | HsVar _ (L _ ann_name) <- expr
    = (name,) <$> if
        | ann_name `matchesThName` 'Loopbreaker   -> Just Loopbreaker
        | ann_name `matchesThName` 'NoLoopbreaker -> Just NoLoopbreaker
        | otherwise                               -> Nothing
  annToTup _ = Nothing

------------------------------------------------------------------------------
matchesThName :: Name -> TH.Name -> Bool
matchesThName name th_name = nameRdrName name `elem` thRdrNameGuesses th_name

------------------------------------------------------------------------------
-- | Inserts loopbreaker to recursive binding group of single binding and
-- emits necessary signatures.
inlineRecCall
  :: MonadInline m
  => Bool                           -- ^ inline by default?
  -> Map Name (LHsSigWcType GhcRn)  -- ^ types of bindings
  -> Map Name LoopbreakerAnn        -- ^ 'Loopbreaker' annotations
  -> (RecFlag, LHsBinds GhcRn)      -- ^ binding being inlined
  -> m ((RecFlag, LHsBinds GhcRn), [LSig GhcRn])
inlineRecCall isDefaultOn types anns (Recursive, binds)
  | (bagToList -> [L fun_loc fun_bind])           <- binds
  , FunBind{ fun_id = L _ fun_name, fun_matches } <- fun_bind
  , ann                                           <- M.lookup fun_name anns
  , ann == Just Loopbreaker || ann == Nothing && isDefaultOn
  = do
  dyn_flags <- getDynFlags
  liftIO $ debugTraceMsg dyn_flags 2 $ text "Loopbreaker:" <+> ppr fun_name

  (loopb_name, loopb_decl) <- loopbreaker fun_name

  let m_loopb_sig  = loopbreakerSig loopb_name <$> M.lookup fun_name types
      fun_matches' = replaceVarNames fun_name loopb_name fun_matches

  pure
    ( ( Recursive
      , listToBag
          [ L fun_loc fun_bind{ fun_matches = fun_matches' }
          , loopb_decl
          ]
      )
    , (  [ inlineSig alwaysInlinePragma fun_name
         , inlineSig noInlinePragma     loopb_name
         ]
      -- If the original function didn't have type signature specified, we
      -- shouldn't have to have either
      ++ maybeToList m_loopb_sig
      )
    )
-- We ignore mutually recursive and other bindings
inlineRecCall _ _ _ binds = pure (binds, [])

------------------------------------------------------------------------------
-- | Creates loopbreaker and it's name from the name of original function.
loopbreaker :: MonadUnique m => Name -> m (Name, LHsBind GhcRn)
loopbreaker fun_name =
  (id &&& loopbreakerDecl fun_name) <$> loopbreakerName fun_name

------------------------------------------------------------------------------
loopbreakerName :: MonadUnique m => Name -> m Name
loopbreakerName (occName -> occNameFS -> orig_fs) =
  flip mkSystemVarName (orig_fs <> "__Loopbreaker") <$> getUniqueM

------------------------------------------------------------------------------
loopbreakerDecl :: Name -> Name -> LHsBind GhcRn
loopbreakerDecl fun_name loopb_name =
  noLoc $ mkTopFunBind Generated (noLoc loopb_name)
    [ mkSimpleMatch (mkPrefixFunRhs $ noLoc loopb_name) [] $
        nlHsVar fun_name
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
-- | Contrary to 'neverInlinePragma', this has behaviour of 'NOINLINE' pragma.
noInlinePragma :: InlinePragma
noInlinePragma = defaultInlinePragma
  { inl_inline = NoInline
  , inl_act    = NeverActive
  }

------------------------------------------------------------------------------
-- | Returns value with every name in variable expression replaced.
replaceVarNames :: Data a => Name -> Name -> a -> a
replaceVarNames from to = everywhere $ mkT $ \case
  HsVar NoExt (L loc name) :: HsExpr GhcRn
    | name == from -> HsVar NoExt $ L loc to
  e -> e
