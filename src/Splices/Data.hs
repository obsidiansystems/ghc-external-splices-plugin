{-# LANGUAGE DeriveGeneric #-}
module Splices.Data where

import BinIface
import qualified Binary as Bin
import DynFlags
import ErrUtils
import FastMutInt
import GHC.Hs.Decls
import GHC.Hs.Expr
import GHC.Hs.Extension
import GHC.Hs.Pat
import GHC.Hs.Types
import IfaceEnv
import Module
import Outputable
import SrcLoc
import TcRnTypes
import UniqFM

import Splices.Conversions
import Splices.Serialise.Class
import Splices.Serialise.Instances ()

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import GHC.Generics (Generic)
import qualified Data.Map as Map
import Prelude hiding ((<>))
import System.Directory
import System.FilePath

data SpliceResult
  = SRExpr  (LHsExpr GhcPs)
  | SRDecls [LHsDecl GhcPs]
  | SRPat   (LPat GhcPs)
  | SRTy    (LHsType GhcPs)
  deriving Generic

instance Serialise SpliceResult

newtype HsSpliceData = HsSpliceData
  { hsSpliceMap :: Map.Map SrcSpan SpliceResult }

instance Serialise HsSpliceData where
  sput bh (HsSpliceData m) = sput bh (Map.toList m)
  sget bh = fmap (HsSpliceData . Map.fromList) (sget bh)

emptyHsSpliceData :: HsSpliceData
emptyHsSpliceData = HsSpliceData Map.empty

nonEmptyHsSpliceData :: HsSpliceData -> Bool
nonEmptyHsSpliceData = not . Map.null . hsSpliceMap

readHsSpliceData :: FilePath -> TcM HsSpliceData
readHsSpliceData fp = do
  exists <- liftIO (doesFileExist fp)
  if exists
    then do bh <- liftIO (Bin.readBinMem fp)
            ncu <- mkNameCacheUpdater
            checkedConv $ getWithUserData' ncu bh sget
    else pure emptyHsSpliceData

writeHsSpliceData :: FilePath -> HsSpliceData -> TcM ()
writeHsSpliceData fp splicedata =
  when (nonEmptyHsSpliceData splicedata) $ do
    dflags <- getDynFlags
    bh <- liftIO $ Bin.openBinMem (100 * 1024)
    --  ^^^ FIXME: how should we compute an approximation of size?
    checkedConv $ putWithUserData' (debugTraceMsg dflags 3) bh splicedata sput
    liftIO (Bin.writeBinMem bh fp)

getModuleSplicesPath :: FilePath -> Module -> FilePath
getModuleSplicesPath splicesDir m = splicesDir
  </> toPath (moduleNameString (moduleName m)) <.> "hs-splice"

  where toPath = map (\c -> if c == '.' then '/' else c)

recordSpliceResult
  :: SrcSpan -> SpliceResult -> HsSpliceData -> HsSpliceData
recordSpliceResult loc res (HsSpliceData m) = HsSpliceData (Map.insert loc res m)

lookupSpliceResult :: SrcSpan -> HsSpliceData -> Maybe SpliceResult
lookupSpliceResult loc (HsSpliceData m) = Map.lookup loc m

checkedConv :: Conv a -> RnM a
checkedConv (Conv m) = do
  res <- m
  case res of
    ConvOK a -> pure a
    ConvError (ConvUnsupported conName tyName expr) ->
      pprPanic "Splices.Data.checkedConv" $ vcat
        [ text "GHC encountered a Haskell construct not supported by the splices plugin"
        , nest 4 expr
        , nest 4 (text "constructor " <> text conName <> text " of type " <> text tyName)
        ]
    ConvError (ConvBadTag ty tag) ->
      pprPanic "SpliceData.handleUnsupported" . vcat $
        [ text "GHC encountered an unknown constructor tag:"
        , nest 4 $ text (show tag)
        , text "for type " <> quote (text ty)
        ]
    ConvError (ConvFailure s) -> panic s

-- | Panics with a nice error when we encounter an unsupported
--   construct, or returns the actual result if the conversion
--   succeeded.
handleUnsupported
  :: Located SDoc -- ^ TH expression that got evaluated
  -> Maybe SDoc -- ^ code resulting from the evaluation of the 1st arg
  -> ConvResult a -- ^ result of the conversion
  -> RnM a
handleUnsupported (L loc thDoc) resDoc convRes = case convRes of
  ConvOK a -> pure a
  ConvError (ConvUnsupported conName tyName subexprDoc) ->
    pprPanic "SpliceData.handleUnsupported" . vcat $
      [ text "GHC encountered a Haskell construct not supported by -{load, save}-splices:"
      , nest 4 $ subexprDoc <> text (" - constructor " ++ conName ++ " of type " ++ tyName)
      , text "while evaluating the following expression from "  <> ppr loc <> text ":"
      , nest 4 $ thDoc
      ] ++
      maybe [] (\d -> [text "which resulted in:" , nest 4 d]) resDoc
  ConvError (ConvBadTag ty tag) ->
    pprPanic "SpliceData.handleUnsupported" . vcat $
      [ text "GHC encountered an unknown constructor tag:"
      , nest 4 $ text (show tag ++ " for type " ++ ty)
      , text "while decoding a Haskell AST from an .hs-splice file"
      , text "for splice at " <> ppr loc <> text ":"
      , nest 4 $ thDoc
      ]
  ConvError (ConvFailure errorStr) -> panic errorStr

-- {get, put}WithUserData'

putWithUserData'
  :: MonadIO m
  => (SDoc -> IO ()) -> Bin.BinHandle -> a
  -> (Bin.BinHandle -> a -> m ())
  -> m ()
putWithUserData' log_act bh payload putThing = do
    -- Remember where the dictionary pointer will go
    dict_p_p <- liftIO (Bin.tellBin bh)
    -- Placeholder for ptr to dictionary
    liftIO (Bin.put_ bh dict_p_p)

    -- Remember where the symbol table pointer will go
    symtab_p_p <- liftIO (Bin.tellBin bh)
    liftIO (Bin.put_ bh symtab_p_p)
    -- Make some initial state
    symtab_next <- liftIO newFastMutInt
    liftIO (writeFastMutInt symtab_next 0)
    symtab_map <- liftIO (newIORef emptyUFM)
    let bin_symtab = BinSymbolTable {
                         bin_symtab_next = symtab_next,
                         bin_symtab_map  = symtab_map }
    dict_next_ref <- liftIO newFastMutInt
    liftIO (writeFastMutInt dict_next_ref 0)
    dict_map_ref <- liftIO (newIORef emptyUFM)
    let bin_dict = BinDictionary {
                       bin_dict_next = dict_next_ref,
                       bin_dict_map  = dict_map_ref }

    -- Put the main thing,
    bh' <- return $ Bin.setUserData bh $
      Bin.newWriteState (putName bin_dict bin_symtab)
                        (putName bin_dict bin_symtab)
                        (putFastString bin_dict)
    putThing bh' payload

    -- Write the symtab pointer at the front of the file
    symtab_p <- liftIO (Bin.tellBin bh')       -- This is where the symtab will start
    liftIO (Bin.putAt bh' symtab_p_p symtab_p) -- Fill in the placeholder
    liftIO (Bin.seekBin bh' symtab_p)          -- Seek back to the end of the file

    -- Write the symbol table itself
    symtab_next' <- liftIO (readFastMutInt symtab_next)
    symtab_map'  <- liftIO (readIORef symtab_map)
    liftIO (putSymbolTable bh' symtab_next' symtab_map')
    liftIO $ log_act (text "writeBinIface:" <+> int symtab_next'
                                            <+> text "Names")

    -- NB. write the dictionary after the symbol table, because
    -- writing the symbol table may create more dictionary entries.

    -- Write the dictionary pointer at the front of the file
    dict_p <- liftIO (Bin.tellBin bh')     -- This is where the dictionary will start
    liftIO (Bin.putAt bh' dict_p_p dict_p) -- Fill in the placeholder
    liftIO (Bin.seekBin bh' dict_p)        -- Seek back to the end of the file

    -- Write the dictionary itself
    dict_next <- liftIO (readFastMutInt dict_next_ref)
    dict_map  <- liftIO (readIORef dict_map_ref)
    liftIO $ Bin.putDictionary bh' dict_next dict_map
    liftIO $ log_act (text "writeBinIface:" <+> int dict_next
                                            <+> text "dict entries")


getWithUserData'
  :: MonadIO m
  => NameCacheUpdater
  -> Bin.BinHandle
  -> (Bin.BinHandle -> m a)
  -> m a
getWithUserData' ncu bh getThing = do
    -- Read the dictionary
    -- The next word in the file is a pointer to where the dictionary is
    -- (probably at the end of the file)
    dict <- liftIO $ do
      dict_p <- Bin.get bh
      data_p <- Bin.tellBin bh          -- Remember where we are now
      Bin.seekBin bh dict_p
      dict   <- Bin.getDictionary bh
      Bin.seekBin bh data_p             -- Back to where we were before
      return dict

    -- Initialise the user-data field of bh
    bh'' <- liftIO $ do
        bh' <- return $ Bin.setUserData bh $
          Bin.newReadState (error "getSymtabName")
                           (getDictFastString dict)
        symtab_p <- Bin.get bh'     -- Get the symtab ptr
        data_p <- Bin.tellBin bh'          -- Remember where we are now
        Bin.seekBin bh' symtab_p
        symtab <- getSymbolTable bh' ncu
        Bin.seekBin bh' data_p             -- Back to where we were before

        -- It is only now that we know how to get a Name
        return $ Bin.setUserData bh' $
          Bin.newReadState (getSymtabName ncu dict symtab)
                           (getDictFastString dict)

    -- Read the interface file
    getThing bh''
