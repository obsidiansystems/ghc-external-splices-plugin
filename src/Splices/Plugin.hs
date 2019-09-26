module Splices.Plugin where

import GhcPlugins
import GHC.Hs.Expr
import GHC.Hs.Extension
import Hooks
import TcRnTypes
import TcSplice

import Splices.Data

import System.Directory

plugin :: Plugin
plugin = defaultPlugin
  { dynflagsPlugin = \opts -> Just (registerHook opts) }

registerHook :: [String] -> (DynFlags -> IO DynFlags)
registerHook opts = \dflags -> do
  createDirectoryIfMissing True (modeDir mode)
  return $ dflags
    { hooks = (hooks dflags)
      { runMetaHook = Just $ splicesHook mode defaultRunMeta }
    }

  where mode = parseOpts opts

splicesHook :: Mode -> MetaHook TcM -> MetaHook TcM
splicesHook mode metaHook req e = case mode of
  Load dir -> load dir
  Save dir
    | isAW req  -> metaHook req e
    | otherwise -> save dir

  where load d = do
          splicedata <- readHsSpliceData =<< spliceFilePath d
          readSpliceFor (loc e) splicedata req

        save d = do
          (spliceres, res) <- runMHook metaHook req e
          fp <- spliceFilePath d
          splicedata <- readHsSpliceData fp
          let splicedata' = recordSpliceResult (loc e) spliceres splicedata
          writeHsSpliceData fp splicedata'
          return res

        spliceFilePath d = do
          mdl <- getModule
          return (getModuleSplicesPath d mdl)

        loc (L l _) = l

        isAW (MetaAW _) = True
        isAW _ = False

runMHook
  :: MetaHook TcM -> MetaRequest -> LHsExpr GhcTc
  -> TcM (SpliceResult, MetaResult)
runMHook h req e = case req of
  MetaE k -> do
    resE <- metaRequestE h e
    pure (SRExpr resE, k resE)
  MetaD k -> do
    resDs <- metaRequestD h e
    pure (SRDecls resDs, k resDs)
  MetaT k -> do
    resT <- metaRequestT h e
    pure (SRTy resT, k resT)
  MetaP k -> do
    resP <- metaRequestP h e
    pure (SRPat resP, k resP)
  MetaAW _ -> panic "runMHook: MetaAW -> impossible"

readSpliceFor
  :: SrcSpan
  -> HsSpliceData
  -> MetaRequest
  -> TcM MetaResult
readSpliceFor loc splicedata req =
  case lookupSpliceResult loc splicedata of
    Nothing -> panic ("Could not find splice result for source span " ++ show loc)
    Just r -> pure (go r req)

  where go (SRExpr e) (MetaE k) = k e
        go (SRDecls ds) (MetaD k) = k ds
        go (SRPat p) (MetaP k) = k p
        go (SRTy t) (MetaT k) = k t
        go _ (MetaAW k) = k (toSerialized serializeWithData ())
        go sres metareq = mismatch sres metareq

        mismatch sres metareq = panic $
          "readSpliceFor: couldn't match " ++ srstr sres ++ " splice result" ++
          " with " ++ mrstr metareq ++ " meta request"

        srstr (SRExpr _) = "expression"
        srstr (SRDecls _) = "declarations"
        srstr (SRPat _) = "pattern"
        srstr (SRTy _) = "type"

        mrstr (MetaE _) = "expression"
        mrstr (MetaD _) = "declarations"
        mrstr (MetaP _) = "pattern"
        mrstr (MetaT _) = "type"
        mrstr _         = panic "mrstr: impossible"

data Mode = Load FilePath | Save FilePath
  deriving (Eq, Show)

modeDir :: Mode -> FilePath
modeDir (Load d) = d
modeDir (Save d) = d

parseOpts :: [CommandLineOption] -> Mode
parseOpts ["save", dir] = Save dir
parseOpts ["load", dir] = Load dir
parseOpts opts = error $
  "Splices.Plugin.parseOpts: unknown options " ++ show opts
