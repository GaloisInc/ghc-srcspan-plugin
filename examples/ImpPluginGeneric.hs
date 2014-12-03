{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module ImpPluginGeneric (plugin) where

import           GHC.Plugins.SrcSpan

import           DynamicLoading
import           GhcPlugins

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
  reinitializeGlobals

  hsc_env <- getHscEnv

  Just withLocName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env iMP_mod wITH_LOC
  withLocVar <- lookupId withLocName

  Just mkLocName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env iMP_mod mK_LOC
  mkLocVar <- lookupId mkLocName

  Just impName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env iMP_mod iMP
  impCon <- lookupTyCon impName

  let isInteresting expr = return (isImpStmt impCon expr)
  let annotate loc expr = mkWithLocExpr mkLocVar withLocVar loc expr
  let locpass = mkPass isInteresting annotate killForeignStubs

  return $ (CoreDoPluginPass "Add Locations" locpass) : todos
  where
  killForeignStubs = "kill-foreign-stubs" `elem` opts


isImpStmt :: TyCon -> CoreExpr -> Bool
isImpStmt impTyCon expr
  | Just (tc, _) <- splitTyConApp_maybe $ exprType expr
  = tc == impTyCon
  | otherwise
  = False


mkWithLocExpr :: Var -> Var -> SrcSpan -> CoreExpr -> CoreM CoreExpr
mkWithLocExpr mkLocVar withLocVar (RealSrcSpan ss) expr = do
  loc <- mkLocExpr mkLocVar ss
  return $ mkCoreApps (Var withLocVar) $ map Type tys ++ [ loc, expr ]
  where
  (_, tys) = splitAppTys $ exprType expr

mkWithLocExpr _ _ _ expr = return expr


mkLocExpr :: Var -> RealSrcSpan -> CoreM CoreExpr
mkLocExpr mkLocVar ss = do
  df   <- getDynFlags
  file <- mkStringExprFS $ srcSpanFile ss
  return $ mkCoreApps (Var mkLocVar) [ file
                                     , mkIntExprInt df (srcSpanStartLine ss)
                                     , mkIntExprInt df (srcSpanStartCol ss)
                                     , mkIntExprInt df (srcSpanEndLine ss)
                                     , mkIntExprInt df (srcSpanEndCol ss)
                                     ]

iMP_mod :: ModuleName
iMP_mod = mkModuleName "Imp"

wITH_LOC, mK_LOC, iMP :: RdrName
wITH_LOC = mkVarUnqual $ fsLit "withLocation"
mK_LOC   = mkVarUnqual $ fsLit "makeLocation"
iMP      = mkRdrQual iMP_mod $ mkTcOcc "Imp"
