module Loopbreaker.InlineRecCalls.CoreToDos (installToDos) where

import Data.Bool

import BasicTypes
import CoreMonad
import DynFlags


------------------------------------------------------------------------------
installToDos :: [CoreToDo] -> CoreM [CoreToDo]
installToDos to_dos = do
  dyn_flags <- getDynFlags

  pure $ to_dos ++ bool [] (extraPhases dyn_flags) (optLevel dyn_flags > 0)

------------------------------------------------------------------------------
extraPhases :: DynFlags -> [CoreToDo]
extraPhases = do
  max_iters  <- maxSimplIterations
  phases_num <- simplPhases

  simplify_phase  <- simplifyPhase 0 ["post-late-spec"] max_iters
  simplify_gently <- simplifyGently max_iters
  simplify_phases <- simplifyPhases phases_num max_iters

  pure
    [ CoreDoSpecialising
    , simplify_phase
    , simplify_gently
    , CoreDoStaticArgs
    , CoreDoSpecialising
    -- TODO: We don't need this one?
    , simplify_phase
    , simplify_phases
    , simplify_gently
    ]

------------------------------------------------------------------------------
simplifyPhases :: Int -> Int -> DynFlags -> CoreToDo
simplifyPhases phases_num max_iters dyn_flags = CoreDoPasses
  [ simplifyPhase phase_num ["main"] max_iters dyn_flags
  | phase_num <- [phases_num, phases_num - 1 .. 1]
  ]

------------------------------------------------------------------------------
-- TODO: some description from Sandy
simplifyPhase :: Int -> [String] -> Int -> DynFlags -> CoreToDo
simplifyPhase phase_num names max_iters dyn_flags = CoreDoPasses
  [ runWhen (phase_num `elem` strictnessBefore dyn_flags) CoreDoStrictness
  , CoreDoSimplify max_iters (baseSimplMode dyn_flags){
        sm_phase = Phase phase_num
      , sm_names = names
      }
  , runMaybe (ruleCheck dyn_flags) $ CoreDoRuleCheck $ Phase phase_num
  ]

------------------------------------------------------------------------------
simplifyGently :: Int -> DynFlags -> CoreToDo
simplifyGently max_iters dyn_flags =
  CoreDoSimplify max_iters (baseSimplMode dyn_flags){
      sm_phase     = InitialPhase
    , sm_names     = ["Gentle"]
    , sm_rules     = gopt Opt_DoLambdaEtaExpansion dyn_flags
    , sm_inline    = True
    , sm_case_case = False
    }

------------------------------------------------------------------------------
baseSimplMode :: DynFlags -> SimplMode
baseSimplMode dyn_flags = SimplMode
  { sm_phase      = error "base_mode"
  , sm_names      = []
  , sm_dflags     = dyn_flags
  , sm_rules      = gopt Opt_EnableRewriteRules   dyn_flags
  , sm_eta_expand = gopt Opt_DoLambdaEtaExpansion dyn_flags
  , sm_inline     = True
  , sm_case_case  = True
  }
