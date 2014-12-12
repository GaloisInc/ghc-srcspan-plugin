{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module ImpPluginExplicit (plugin) where

import           Control.Exception
import           Control.Monad
import qualified Data.Array        as Array
import           Data.IntMap       (IntMap)
import qualified Data.IntMap       as IntMap
import           Trace.Hpc.Mix
import           Trace.Hpc.Util

import           CostCentre
import           GhcPlugins

import qualified Imp

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todos = do
  reinitializeGlobals
  return $ locs : todos
  where 
  locs = CoreDoPluginPass "Add Locations" $ pass

pass :: ModGuts -> CoreM ModGuts
pass guts@(ModGuts {..}) = do
  hsc_env <- getHscEnv
  df  <- getDynFlags

  tickSpan <- liftIO $ getSpans df guts

  Just withLocName <- thNameToGhcName 'Imp.withLocation
  withLocId <- lookupId withLocName

  Just mkLocName <- thNameToGhcName 'Imp.makeLocation
  mkLocId <- lookupId mkLocName

  Just impName <- thNameToGhcName ''Imp.Imp
  impTyCon <- lookupTyCon impName

  binds <- mapM (addLocationsBind tickSpan
                 (annotate impTyCon mkLocId withLocId))
                mg_binds

  return (guts { mg_binds = binds })


getSpans :: DynFlags -> ModGuts -> IO (Tickish Id -> SrcSpan)
getSpans df ModGuts {..} = do
  let modName = moduleName mg_module
  mixEntries <- getMixEntries modName (hpcDir df) 
                  `catch` \(_ :: SomeException) -> return []
  let hpc = IntMap.fromList $ zip [0..] mixEntries
  let bk  = IntMap.fromList $ Array.assocs $ modBreaks_locs mg_modBreaks
  return (tickSpan hpc bk)


getMixEntries :: ModuleName -> FilePath -> IO [SrcSpan]
getMixEntries nm dir = do
  Mix file _ _ _ entries <- readMix [dir] (Left $ moduleNameString nm)
  let f = fsLit file
  return [ mkSrcSpan (mkSrcLoc f l1 c1) (mkSrcLoc f l2 c2) 
         | (hpc, _) <- entries
         , let (l1,c1,l2,c2) = fromHpcPos hpc 
         ]


tickSpan :: IntMap SrcSpan -> IntMap SrcSpan -> Tickish Id -> SrcSpan
tickSpan _   _  (ProfNote cc _ _) = cc_loc cc
tickSpan hpc _  (HpcTick _ i)     = IntMap.findWithDefault noSrcSpan i hpc
tickSpan _   bk (Breakpoint i _)  = IntMap.findWithDefault noSrcSpan i bk


addLocationsBind :: (Tickish Var -> SrcSpan)
                 -> (SrcSpan -> CoreExpr -> CoreM CoreExpr) 
                 -> CoreBind -> CoreM CoreBind
addLocationsBind getSpan annotate bndr = case bndr of
  NonRec b expr -> NonRec b `liftM` addLocationsExpr getSpan annotate expr
  Rec binds     -> Rec `liftM` forM binds (secondM $ addLocationsExpr getSpan annotate)


addLocationsExpr :: (Tickish Var -> SrcSpan)
                 -> (SrcSpan -> CoreExpr -> CoreM CoreExpr)
                 -> CoreExpr -> CoreM CoreExpr
addLocationsExpr getSpan annotate = go noSrcSpan
  where
  go ss (Tick t expr) 
    | isGoodSrcSpan (getSpan t)
    = liftM (Tick t) (go (getSpan t) expr)
    | otherwise
    = liftM (Tick t) (go ss expr)
  go ss e
    = annotate ss =<< to ss e

  to ss (App f e)
    = liftM2 App (go ss f) (go ss e)
  to ss (Lam x expr)
    = liftM (Lam x) (go ss expr)
  to ss (Let bndr expr)
    = liftM2 Let (addLocationsBind getSpan annotate bndr) (go ss expr)
  to ss (Case expr x t alts)
    = liftM2 (\e as -> Case e x t as) (go ss expr) (mapM (addLocationsAlt ss) alts)
  to ss (Cast expr c)
    = liftM (`Cast` c) (go ss expr)
  to _  expr
    = return expr

  addLocationsAlt ss (c, xs, expr)
    = (c, xs,) `liftM` go ss expr


isInteresting :: TyCon -> CoreExpr -> Bool
isInteresting impTyCon expr@(App _ _)
  | Just (tc, _) <- splitTyConApp_maybe $ exprType expr
  = tc == impTyCon
  | otherwise
  = False
isInteresting _ _ = False


annotate :: TyCon -> Id -> Id -> SrcSpan -> CoreExpr -> CoreM CoreExpr
annotate impTyCon mkLocId withLocId (RealSrcSpan ss) expr
  | isInteresting impTyCon expr = do
      loc <- mkLocExpr mkLocId ss
      return $ mkCoreApps (Var withLocId) $ map Type tys ++ [ loc, expr ]
  where
  Just (_, tys) = splitTyConApp_maybe $ exprType expr

annotate _ _ _ _ expr = return expr


mkLocExpr :: Id -> RealSrcSpan -> CoreM CoreExpr
mkLocExpr mkLocId ss = do
  df   <- getDynFlags
  file <- mkStringExprFS $ srcSpanFile ss
  return $ mkCoreApps (Var mkLocId) [ file
                                    , mkIntExprInt df (srcSpanStartLine ss)
                                    , mkIntExprInt df (srcSpanStartCol ss)
                                    , mkIntExprInt df (srcSpanEndLine ss)
                                    , mkIntExprInt df (srcSpanEndCol ss)
                                    ]


secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM f (a, b) = (a,) `liftM` f b
