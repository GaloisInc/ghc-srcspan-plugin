module GHC.Plugins.ErrorLoc
  (plugin, errorAt, undefinedAt, fromJustAt)
  where

import DynamicLoading
import GhcPlugins
import GHC.Plugins.SrcSpan

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
  reinitializeGlobals

  hsc_env <- getHscEnv
  errLocM <- lookupModule (mkModuleName "GHC.Plugins.ErrorLoc") Nothing
  
  errorAtVar <- lookupId =<< lookupName errLocM (mkVarOcc "errorAt")
  undefAtVar <- lookupId =<< lookupName errLocM (mkVarOcc "undefinedAt")
  fmjstAtVar <- lookupId =<< lookupName errLocM (mkVarOcc "fromJustAt")

  maybeM   <- lookupModule (mkModuleName "Data.Maybe") Nothing
  fmjstVar <- lookupId =<< lookupName maybeM (mkVarOcc "fromJust")

  let subst = [ (eRROR_ID, errorAtVar), (uNDEFINED_ID, undefAtVar)
              , (fmjstVar, fmjstAtVar)
              ]

  let annotate = mkErrorAt subst

  let mypass = CoreDoPluginPass "Add Locations to `error` calls"
             $ mkPass annotate ("kill-foreign-stubs" `elem` opts)
  return $ mypass : todos

isErrorVar :: [(Var,Var)] -> Var -> Maybe Var
isErrorVar subst v = lookup v subst

mkErrorAt :: [(Var,Var)] -> SrcSpan -> CoreExpr -> CoreM CoreExpr
mkErrorAt subst loc (App (Var v) (Type t))
  | Just v' <- isErrorVar subst v = do
      df <- getDynFlags
      locStr <- mkStringExpr $ showPpr df loc
      return $ mkCoreApps (Var v') [ Type t, locStr ]
mkErrorAt _ _ expr = return expr


errorAt :: String -> String -> a
errorAt loc msg = error (loc ++ ": " ++ msg)
{-# INLINE errorAt #-}

undefinedAt :: String -> a
undefinedAt loc = errorAt loc "Prelude.undefined"
{-# INLINE undefinedAt #-}

fromJustAt :: String -> Maybe a -> a
fromJustAt loc Nothing  = errorAt loc "Maybe.fromJust: Nothing"
fromJustAt _   (Just x) = x
{-# INLINE fromJustAt #-}
