{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | This module provides a generic Core-to-Core pass for annotating Haskell
-- expressions with the original source locations. You can use it to build a GHC
-- Plugin tailored to your own library by providing a predicate to select
-- interesting expressions for annotation and a function to annotate the
-- expressions.
--
-- Example usage:
--
-- > module MyPlugin (plugin) where
-- >
-- > import GhcPlugins
-- > import GHC.Plugins.SrcSpan
-- >
-- > plugin :: Plugin
-- > plugin = defaultPlugin { installCoreToDos = install }
-- > 
-- > install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
-- > install opts todos = do
-- >   reinitializeGlobals
-- >   return $ mypass : todos
-- >   where
-- >   mypass = CoreDoPluginPass "Add Locations" $ mkPass isInteresting annotate False
-- >   isInteresting expr = ...
-- >   annotate expr = ...
--
-- You will need to coax GHC into adding the source information to the Core via
-- 'Tick's. Currently there are three ways to do this:
-- 
-- 1. Load your module in @ghci@.
--
-- 2. Compile your module with @-prof -fprof-auto-calls@. (You can use other
-- profiling options, but that will result in poorer 'Tick' granularity)
--
-- 3. Compile your module with @-fhpc@. Note that this will result in the @hpc@
-- runtime being linked into your program, which is a bit inconvenient. The
-- plugin will prevent this if you pass @True@ instead of @False@ to 'mkPass',
-- but be warned, this will likely break __any__ FFI code your module uses.

module GHC.Plugins.SrcSpan (mkPass) where

import           Control.Exception
import           Control.Monad
import           CostCentre
import qualified Data.Array        as Array
import           Data.IntMap       (IntMap)
import qualified Data.IntMap       as IntMap
import           GhcPlugins
import           Trace.Hpc.Mix
import           Trace.Hpc.Util


-- | Given a way of identifying "interesting" 'CoreExpr's and annotating them
-- with 'SrcSpan's, construct a Core-to-Core pass that traverses all of the
-- 'CoreBind's and annotates the interesting ones.
mkPass :: (CoreExpr -> CoreM Bool)
          -- ^ Should we annotate this 'CoreExpr'?
       -> (SrcSpan -> CoreExpr -> CoreM CoreExpr)
          -- ^ Annotate the 'CoreExpr' with the 'SrcSpan'.
       -> Bool
          -- ^ Should we remove the @hpc@ hooks from the resulting binary?
       -> ModGuts -> CoreM ModGuts
mkPass isInteresting annotate killForeignStubs guts@(ModGuts {..}) = do
  df    <- getDynFlags
  mkLoc <- liftIO $ getSpans df guts

  binds <- mapM (addLocationsBind mkLoc isInteresting annotate) mg_binds

  let stubs = if killForeignStubs
                 then NoStubs
                 else mg_foreign

  return (guts { mg_binds = binds, mg_foreign = stubs })


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
