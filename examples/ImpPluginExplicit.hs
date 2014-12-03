{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module ImpPluginExplicit (plugin) where

import           Control.Exception
import           Control.Monad
import           CostCentre
import qualified Data.Array        as Array
import           Data.IntMap       (IntMap)
import qualified Data.IntMap       as IntMap
import           DynamicLoading
import           GhcPlugins
import           Trace.Hpc.Mix
import           Trace.Hpc.Util

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

  mkLoc <- liftIO $ getSpans df guts

  Just withLocName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env iMP_mod wITH_LOC
  withLocVar <- lookupId withLocName

  Just mkLocName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env iMP_mod mK_LOC
  mkLocVar <- lookupId mkLocName

  Just impName <- liftIO $ lookupRdrNameInModuleForPlugins hsc_env iMP_mod iMP
  impCon <- lookupTyCon impName

  let isInteresting expr = return (isImpStmt impCon expr)
  let annotate loc expr = mkWithLocExpr mkLocVar withLocVar loc expr

  binds <- mapM (addLocationsBind mkLoc isInteresting annotate) mg_binds

  return (guts { mg_binds = binds })


getSpans :: DynFlags -> ModGuts -> IO (Tickish Var -> SrcSpan)
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
         | (hpc, _) <- entries, let (l1,c1,l2,c2) = fromHpcPos hpc 
         ]


tickSpan :: IntMap SrcSpan -> IntMap SrcSpan -> Tickish Var -> SrcSpan
tickSpan _   _  (ProfNote cc _ _) = cc_loc cc
tickSpan hpc _  (HpcTick _ i)     = IntMap.findWithDefault noSrcSpan i hpc
tickSpan _   bk (Breakpoint i _)  = IntMap.findWithDefault noSrcSpan i bk


addLocationsBind :: (Tickish Var -> SrcSpan)
                 -> (CoreExpr -> CoreM Bool) 
                 -> (SrcSpan -> CoreExpr -> CoreM CoreExpr) 
                 -> CoreBind -> CoreM CoreBind
addLocationsBind getSpan isInteresting annotate bndr = case bndr of
  NonRec b expr -> NonRec b `liftM` addLocationsExpr getSpan isInteresting annotate expr
  Rec binds     -> Rec `liftM` forM binds (secondM $ addLocationsExpr getSpan isInteresting annotate)


addLocationsExpr :: (Tickish Var -> SrcSpan)
                 -> (CoreExpr -> CoreM Bool)
                 -> (SrcSpan -> CoreExpr -> CoreM CoreExpr)
                 -> CoreExpr -> CoreM CoreExpr
addLocationsExpr getSpan isInteresting annotate = go noSrcSpan
  where
  go ss (Tick t expr) 
    | isGoodSrcSpan (getSpan t)
    = liftM (Tick t) (go (getSpan t) expr)
    | otherwise
    = liftM (Tick t) (go ss expr)
  go ss e@(App expr arg) 
    = do b <- isInteresting e
         let rest = liftM2 App (go ss expr) (go ss arg)
         if b
           then annotate ss =<< rest
           else rest
  go ss (Lam x expr)
    = liftM (Lam x) (go ss expr)
  go ss (Let bndr expr)
    = liftM2 Let (addLocationsBind getSpan isInteresting annotate bndr) (go ss expr)
  go ss (Case expr x t alts)
    = liftM2 (\e as -> Case e x t as) (go ss expr) (mapM (addLocationsAlt ss) alts)
  go ss (Cast expr c)
    = liftM (`Cast` c) (go ss expr)
  go _  expr
    = return expr

  addLocationsAlt ss (c, xs, expr)
    = (c, xs,) `liftM` go ss expr


secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM f (a, b) = (a,) `liftM` f b


isImpStmt :: TyCon -> CoreExpr -> Bool
isImpStmt impTyCon expr
  | Just (tc, _) <- splitTyConApp_maybe $ exprType expr
  = tc == impTyCon
  | otherwise
  = False


mkWithLocExpr :: Var -> Var -> SrcSpan -> CoreExpr -> CoreM CoreExpr
mkWithLocExpr mkLocVar withLocVar (RealSrcSpan ss) expr = do
  loc <- mkLocExpr mkLocVar ss
  return $ mkCoreApps (Var withLocVar) [ Type exprResTy, loc, expr ]
  where
  (_, [exprResTy]) = splitAppTys $ exprType expr

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
