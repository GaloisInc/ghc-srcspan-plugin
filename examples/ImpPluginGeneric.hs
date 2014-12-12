{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module ImpPluginGeneric (plugin) where

import           GHC.Plugins.SrcSpan

import           GhcPlugins

import qualified Imp

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
  reinitializeGlobals

  hsc_env <- getHscEnv

  Just withLocName <- thNameToGhcName 'Imp.withLocation
  withLocId <- lookupId withLocName

  Just mkLocName <- thNameToGhcName 'Imp.makeLocation
  mkLocId <- lookupId mkLocName

  Just impName <- thNameToGhcName ''Imp.Imp
  impTyCon <- lookupTyCon impName

  let locpass = mkPass (annotate impTyCon mkLocId withLocId) killForeignStubs

  return $ (CoreDoPluginPass "Add Locations" locpass) : todos
  where
  killForeignStubs = "kill-foreign-stubs" `elem` opts


isInteresting :: TyCon -> CoreExpr -> Bool
isInteresting impTyCon expr@(App _ _)
  | Just (tc, _) <- splitTyConApp_maybe $ exprType expr
  = tc == impTyCon
isInteresting impTyCon expr@(Var _)
  | Just (tc, _) <- splitTyConApp_maybe $ exprType expr
  = tc == impTyCon
isInteresting _ _
  = False

annotate :: TyCon -> Var -> Var -> SrcSpan -> CoreExpr -> CoreM CoreExpr
annotate impTyCon mkLocVar withLocVar (RealSrcSpan ss) expr
  | isInteresting impTyCon expr = do
      loc <- mkLocExpr mkLocVar ss
      return $ mkCoreApps (Var withLocVar) $ map Type tys ++ [ loc, expr ]
      where
      (_, tys) = splitAppTys $ exprType expr
annotate _ _ _ _ expr = return expr


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
