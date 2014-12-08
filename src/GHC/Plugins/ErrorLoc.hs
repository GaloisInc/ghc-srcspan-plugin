module GHC.Plugins.ErrorLoc where

import DynamicLoading
import GhcPlugins
import GHC.Plugins.SrcSpan

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do 
  reinitializeGlobals
    
  hsc_env <- getHscEnv
  Just errorAtName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env
                               (mkModuleName "GHC.Plugins.ErrorLoc")
                               (mkVarUnqual $ fsLit "errorAt")
  errorAtVar <- lookupId errorAtName

  Just undefAtName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env
                               (mkModuleName "GHC.Plugins.ErrorLoc")
                               (mkVarUnqual $ fsLit "undefinedAt")
  undefAtVar <- lookupId undefAtName
  let annotate = mkErrorAt errorAtVar undefAtVar

  let mypass = CoreDoPluginPass "Add Locations to `error` calls"
             $ mkPass annotate ("kill-foreign-stubs" `elem` opts)
  return $ mypass : todos


isErrorVar :: Var -> Bool
isErrorVar v = v == eRROR_ID || v == uNDEFINED_ID

mkErrorAt :: Var -> Var -> SrcSpan -> CoreExpr -> CoreM CoreExpr
mkErrorAt errAt undefAt loc (App (Var v) (Type t))
  | isErrorVar v = do
      df <- getDynFlags
      locStr <- mkStringExpr $ showPpr df loc
      let v' = if v == eRROR_ID
                  then errAt
                  else undefAt
      return $ mkCoreApps (Var v') [ Type t, locStr ]
mkErrorAt _ _ _ expr = return expr


errorAt :: String -> String -> a
errorAt loc msg = error (loc ++ ": " ++ msg)
{-# INLINE errorAt #-}

undefinedAt :: String -> a
undefinedAt loc = errorAt loc "Prelude.undefined"
{-# INLINE undefinedAt #-}
