module Loopbreaker.InlineRecCalls (action) where


import           Control.Arrow
import           Data.Bool
import           Data.Generics
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe

import Bag
import GhcPlugins hiding ((<>))
import HsSyn


------------------------------------------------------------------------------
-- | Forces compiler to inline functions by creating loopbreaker with
-- NO_INLINE pragma, changing recursive calls to use it and by adding INLINE
-- pragma to the original function.
action :: (MonadUnique m, HasDynFlags m) => HsGroup GhcRn -> m (HsGroup GhcRn)
action group = do
  dyn_flags <- getDynFlags

  bool pure inlineRecCalls (optLevel dyn_flags > 0) group

------------------------------------------------------------------------------
inlineRecCalls :: MonadUnique m => HsGroup GhcRn -> m (HsGroup GhcRn)
inlineRecCalls group@(hs_valds -> XValBindsLR (NValBinds binds sigs)) = do
  let types = typesMap sigs

  (binds', extra_sigs) <- second concat . unzip
                      <$> traverse (inlineRecCall types) binds

  pure group{ hs_valds = XValBindsLR $ NValBinds binds' $ sigs ++ extra_sigs }

inlineRecCalls _ = error "inlineRecCalls: expected renamed group"

------------------------------------------------------------------------------
typesMap :: Ord (IdP p) => [LSig p] -> Map (IdP p) (LHsSigWcType p)
typesMap = M.fromList . concatMap sigToTups where
  sigToTups (L _ (TypeSig _ names type_)) = (,type_) . unLoc <$> names
  sigToTups _                             = []

------------------------------------------------------------------------------
-- | Takes binding group and type of binding inside if there's only one and
-- returns group with inlining of recursive calls and signatures to be added
-- to the environment.
inlineRecCall :: MonadUnique m
              => Map (IdP GhcRn) (LHsSigWcType GhcRn)
              -> (RecFlag, LHsBinds GhcRn)
              -> m ((RecFlag, LHsBinds GhcRn), [LSig GhcRn])
inlineRecCall types (Recursive, binds)
  | (bagToList -> [L fun_loc fun_bind])           <- binds
  , FunBind{ fun_id = L _ fun_name, fun_matches } <- fun_bind
  = do
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
    , (  [ inline alwaysInlinePragma fun_name
         , inline noInlinePragma     loopb_name
         ]
      -- If the original function didn't have type signature specified, we
      -- shouldn't have to have either
      ++ maybeToList m_loopb_sig
      )
    )
-- We ignore mutually recursive and other bindings
inlineRecCall _ binds = pure (binds, [])

------------------------------------------------------------------------------
-- | Creates loopbreaker and it's name from name of original function.
loopbreaker :: MonadUnique m => Name -> m (Name, LHsBind GhcRn)
loopbreaker fun_name =
  (id &&& loopbreakerDecl fun_name) <$> loopbreakerName fun_name

------------------------------------------------------------------------------
loopbreakerName :: MonadUnique m => Name -> m Name
loopbreakerName (nameOccName -> occNameFS -> orig_fs) =
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
inline :: (XInlineSig p ~ NoExt) => InlinePragma -> IdP p -> LSig p
inline how name = noLoc $ InlineSig NoExt (noLoc name) how

------------------------------------------------------------------------------
-- | Contrary to 'neverInlinePragma', this has behaviour of 'NOINLINE' pragma.
noInlinePragma :: InlinePragma
noInlinePragma = defaultInlinePragma
  { inl_inline = NoInline
  , inl_act    = NeverActive
  }

------------------------------------------------------------------------------
-- | Returns value with every variable expression id replaced.
replaceVarNames :: Data a => Name -> Name -> a -> a
replaceVarNames from to = everywhere $ mkT $ \case
  HsVar NoExt (L loc name) :: HsExpr GhcRn
    | name == from -> HsVar NoExt $ L loc to
  e -> e
