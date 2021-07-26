{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Splices.Serialise.Instances where

import GhcPrelude

import Bag
import BasicTypes
import BooleanFormula
import CoreSyn
import CostCentre
import FastString
import ForeignCall
import GHC.Hs.Binds
import GHC.Hs.Decls
import GHC.Hs.Doc
import GHC.Hs.Expr
import GHC.Hs.Extension
import GHC.Hs.Lit
import GHC.Hs.Pat
import GHC.Hs.Types
import IfaceType
import Module
import Name
import Outputable
import RdrName
import SrcLoc
import TcEvidence
import Unique

import Splices.Conversions
import Splices.Serialise.Class

import GHC.Generics (Generic)
import GHC.Real

deriving instance Generic NoExtField
instance Serialise NoExtField

deriving instance Generic NoExtCon
instance Serialise NoExtCon

instance Serialise (HsExpr GhcPs) where
  sput bh expr = case expr of
    HsVar a b -> do
      putTag bh 0
      sput bh a >> sput bh b
    HsRecFld a b -> do
      putTag bh 1
      sput bh a >> sput bh b
    HsOverLabel a b c -> do
      putTag bh 2
      sput bh a >> sput bh b >> sput bh c
    HsIPVar a b -> do
      putTag bh 3
      sput bh a >> sput bh b
    HsOverLit a b -> do
      putTag bh 4
      sput bh a >> sput bh b
    HsLit a b -> do
      putTag bh 5
      sput bh a >> sput bh b
    HsLam a b -> do
      putTag bh 6
      sput bh a >> sput bh b
    HsLamCase a b -> do
      putTag bh 7
      sput bh a >> sput bh b
    HsApp a b c -> do
      putTag bh 8
      sput bh a >> sput bh b >> sput bh c
    HsAppType a b c -> do
      putTag bh 9
      sput bh a >> sput bh b >> sput bh c
    OpApp a b c d -> do
      putTag bh 10
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    NegApp a b c -> do
      putTag bh 11
      sput bh a >> sput bh b >> sput bh c
    HsPar a b -> do
      putTag bh 12
      sput bh a >> sput bh b
    SectionL a b c -> do
      putTag bh 13
      sput bh a >> sput bh b >> sput bh c
    SectionR a b c -> do
      putTag bh 14
      sput bh a >> sput bh b >> sput bh c
    ExplicitTuple a b c -> do
      putTag bh 15
      sput bh a >> sput bh b >> sput bh c
    ExplicitSum a b c d -> do
      putTag bh 16
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    HsCase a b c -> do
      putTag bh 17
      sput bh a >> sput bh b >> sput bh c
    HsIf a b c d e -> do
      putTag bh 18
      sput bh a >> sput bh b >> sput bh c >> sput bh d >> sput bh e
    HsMultiIf a b -> do
      putTag bh 19
      sput bh a >> sput bh b
    HsLet a b c -> do
      putTag bh 20
      sput bh a >> sput bh b >> sput bh c
    HsDo a b c -> do
      putTag bh 21
      sput bh a >> sput bh b >> sput bh c
    ExplicitList a b c -> do
      putTag bh 22
      sput bh a >> sput bh b >> sput bh c
    RecordCon a b c -> do
      putTag bh 23
      sput bh a >> sput bh b >> sput bh c
    RecordUpd a b c -> do
      putTag bh 24
      sput bh a >> sput bh b >> sput bh c
    ExprWithTySig a b c -> do
      putTag bh 25
      sput bh a >> sput bh b >> sput bh c
    ArithSeq a b c -> do
      putTag bh 26
      sput bh a >> sput bh b >> sput bh c
    HsSCC a b c d -> do
      putTag bh 27
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    HsCoreAnn a b c d -> do
      putTag bh 28
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    HsBracket a b -> do
      putTag bh 29
      sput bh a >> sput bh b
    HsSpliceE a b -> do
      putTag bh 30
      sput bh a >> sput bh b
    HsProc a b c -> do
      putTag bh 31
      sput bh a >> sput bh b >> sput bh c
    HsStatic a b -> do
      putTag bh 32
      sput bh a >> sput bh b
    HsTick a b c -> do
      putTag bh 33
      sput bh a >> sput bh b >> sput bh c
    HsBinTick a b c d -> do
      putTag bh 34
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    HsTickPragma a b c d e -> do
      putTag bh 35
      sput bh a >> sput bh b >> sput bh c >> sput bh d >> sput bh e
    XExpr a -> do
      putTag bh 36
      sput bh a

    HsUnboundVar {} -> unsupported "HsUnboundVar" "HsExpr" (ppr expr)
    HsConLikeOut {} -> unsupported "HsConLikeOut" "HsExpr" (ppr expr)
    HsRnBracketOut {} -> unsupported "HsRnBracketOut" "HsExpr" (ppr expr)
    HsTcBracketOut {} -> unsupported "HsTcBracketOut" "HsExpr" (ppr expr)
    HsWrap {} -> unsupported "HsWrap" "HsExpr" (ppr expr)

  sget bh = do
    tag <- getTag bh
    case tag of
      0 -> HsVar <$> sget bh <*> sget bh
      1 -> HsRecFld <$> sget bh <*> sget bh
      2 -> HsOverLabel <$> sget bh <*> sget bh <*> sget bh
      3 -> HsIPVar <$> sget bh <*> sget bh
      4 -> HsOverLit <$> sget bh <*> sget bh
      5 -> HsLit <$> sget bh <*> sget bh
      6 -> HsLam <$> sget bh <*> sget bh
      7 -> HsLamCase <$> sget bh <*> sget bh
      8 -> HsApp <$> sget bh <*> sget bh <*> sget bh
      9 -> HsAppType <$> sget bh <*> sget bh <*> sget bh
      10 -> OpApp <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      11 -> NegApp <$> sget bh <*> sget bh <*> sget bh
      12 -> HsPar <$> sget bh <*> sget bh
      13 -> SectionL <$> sget bh <*> sget bh <*> sget bh
      14 -> SectionR <$> sget bh <*> sget bh <*> sget bh
      15 -> ExplicitTuple <$> sget bh <*> sget bh <*> sget bh
      16 -> ExplicitSum <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      17 -> HsCase <$> sget bh <*> sget bh <*> sget bh
      18 -> HsIf <$> sget bh <*> sget bh <*> sget bh <*> sget bh <*> sget bh
      19 -> HsMultiIf <$> sget bh <*> sget bh
      20 -> HsLet <$> sget bh <*> sget bh <*> sget bh
      21 -> HsDo <$> sget bh <*> sget bh <*> sget bh
      22 -> ExplicitList <$> sget bh <*> sget bh <*> sget bh
      23 -> RecordCon <$> sget bh <*> sget bh <*> sget bh
      24 -> RecordUpd <$> sget bh <*> sget bh <*> sget bh
      25 -> ExprWithTySig <$> sget bh <*> sget bh <*> sget bh
      26 -> ArithSeq <$> sget bh <*> sget bh <*> sget bh
      27 -> HsSCC <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      28 -> HsCoreAnn <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      29 -> HsBracket <$> sget bh <*> sget bh
      30 -> HsSpliceE <$> sget bh <*> sget bh
      31 -> HsProc <$> sget bh <*> sget bh <*> sget bh
      32 -> HsStatic <$> sget bh <*> sget bh
      33 -> HsTick <$> sget bh <*> sget bh <*> sget bh
      34 -> HsBinTick <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      35 -> HsTickPragma <$> sget bh <*> sget bh <*> sget bh <*> sget bh <*> sget bh
      36 -> XExpr <$> sget bh
      n -> badTag "HsExpr" n

deriving instance Generic a => Generic (MatchGroup GhcPs a)
instance (Generic a, Serialise a) => Serialise (MatchGroup GhcPs a)

deriving instance Generic (GenLocated l e)
instance (Serialise l, Serialise e) => Serialise (GenLocated l e)

deriving instance Generic (Match GhcPs a)
instance Serialise a => Serialise (Match GhcPs a)

instance Serialise SrcSpan where
  sput = binsput
  sget = binsget

deriving instance Generic Origin
instance Serialise Origin

deriving instance Generic (StmtLR GhcPs GhcPs a)
instance Serialise a => Serialise (StmtLR GhcPs GhcPs a)

deriving instance Generic (GRHSs GhcPs a)
instance Serialise a => Serialise (GRHSs GhcPs a)

deriving instance Generic (GRHS GhcPs a)
instance Serialise a => Serialise (GRHS GhcPs a)

deriving instance Generic (HsLocalBindsLR GhcPs GhcPs)
instance Serialise (HsLocalBindsLR GhcPs GhcPs)

instance Serialise (HsValBindsLR GhcPs GhcPs) where
  sput bh binds = case binds of
    ValBinds a b c -> sput bh a >> sput bh b >> sput bh c
    XValBindsLR {} -> unsupported "XValBindsLR" "HsValBindsLR" (ppr binds)
  sget bh = ValBinds <$> sget bh <*> sget bh <*> sget bh

deriving instance Generic (HsIPBinds GhcPs)
instance Serialise (HsIPBinds GhcPs)

deriving instance Generic (IPBind GhcPs)
instance Serialise (IPBind GhcPs)

deriving instance Generic HsIPName
instance Serialise HsIPName

instance Serialise FastString where
  sput = binsput
  sget = binsget

instance Serialise HsDocString where
  sput = binsput
  sget = binsget

deriving instance Generic PromotionFlag
instance Serialise PromotionFlag

deriving instance Generic HsTupleSort
instance Serialise HsTupleSort

deriving instance Generic HsSrcBang
instance Serialise HsSrcBang

deriving instance Generic HsTyLit
instance Serialise HsTyLit

deriving instance Generic Boxity
instance Serialise Boxity

deriving instance Generic (ParStmtBlock GhcPs GhcPs)
instance Serialise (ParStmtBlock GhcPs GhcPs)

instance Serialise (SyntaxExpr GhcPs) where
  sput bh (SyntaxExpr a [] WpHole) = sput bh a
  sput _ _ = badInput "SyntaxExpr with non-WpHole wrapper"
  sget bh = SyntaxExpr <$> sget bh <*> pure [] <*> pure WpHole

instance Serialise a => Serialise (Bag a) where
  sput bh bag = sput bh (bagToList bag)
  sget bh = listToBag <$> sget bh

instance Serialise RdrName where
  sput bh n = case n of
    Unqual a -> putTag bh 0 >> sput bh a
    Qual a b -> putTag bh 1 >> sput bh a >> sput bh b
    Orig a b -> putTag bh 2 >> sput bh a >> sput bh b
    Exact a
      | isExternalName a -> putTag bh 3 >> sput bh a
      | otherwise -> putTag bh (if isSystemName a then 4 else 5)
                  >> sput bh (nameUnique a)
                  >> sput bh (nameOccName a)
                  >> sput bh (nameSrcSpan a)

  sget bh = do
    tag <- getTag bh
    case tag of
      0 -> Unqual <$> sget bh
      1 -> Qual <$> sget bh <*> sget bh
      2 -> tweakOrig =<< (Orig <$> sget bh <*> sget bh)
      3 -> Exact <$> sget bh
      4 -> fmap Exact (mkSystemNameAt <$> sget bh <*> sget bh <*> sget bh)
      5 -> fmap Exact (mkInternalName <$> sget bh <*> sget bh <*> sget bh)
      n -> badTag "RdrName" n

    where tweakOrig :: RdrName -> Conv RdrName
          tweakOrig n = convertName (SeName n)

instance Serialise Name where
  sput = binsput
  sget = binsget

instance Serialise Unique where
  sput bh u = sput bh (getKey u)
  sget bh = mkUniqueGrimily <$> sget bh

instance Serialise OccName where
  sput = binsput
  sget = binsget

instance Serialise ModuleName where
  sput = binsput
  sget = binsget

instance Serialise Module where
  sput = binsput
  sget = binsget

instance Serialise (HsBindLR GhcPs GhcPs) where
  sput bh bind = case bind of
    FunBind a b c d _e -> case d of
      WpHole -> do
        putTag bh 0
        sput bh a >> sput bh b >> sput bh c
      _ -> badInput "FunBind with HsWrapper that's not WpHole"
    PatBind a b c _d -> do
      putTag bh 1
      sput bh a >> sput bh b >> sput bh c
    VarBind a b c d -> do
      putTag bh 2
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    PatSynBind a b -> do
      putTag bh 3
      sput bh a >> sput bh b
    XHsBindsLR a -> do
      putTag bh 4
      sput bh a
    AbsBinds {} -> unsupported "AbsBinds" "HsBindLR" (ppr bind)

  sget bh = do
    tag <- getTag bh
    case tag of
      0 -> FunBind <$> sget bh <*> sget bh <*> sget bh
                   <*> pure WpHole <*> pure []
      1 -> PatBind <$> sget bh <*> sget bh <*> sget bh
                   <*> pure ([], [])
      2 -> VarBind <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      3 -> PatSynBind <$> sget bh <*> sget bh
      4 -> XHsBindsLR <$> sget bh
      n -> badTag "HsBindLR" n

deriving instance Generic (HsStmtContext a)
instance Serialise a => Serialise (HsStmtContext a)

deriving instance Generic (HsMatchContext a)
instance Serialise a => Serialise (HsMatchContext a)

deriving instance Generic LexicalFixity
instance Serialise LexicalFixity

instance Serialise SrcStrictness where
  sput = binsput
  sget = binsget

deriving instance Generic (FieldOcc GhcPs)
instance Serialise (FieldOcc GhcPs)

deriving instance Generic (HsRecField' a b)
instance (Serialise a, Serialise b) => Serialise (HsRecField' a b)

deriving instance Generic (HsRecFields GhcPs a)
instance Serialise a => Serialise (HsRecFields GhcPs a)

deriving instance Generic (HsWildCardBndrs GhcPs a)
instance Serialise a => Serialise (HsWildCardBndrs GhcPs a)

deriving instance Generic (HsImplicitBndrs GhcPs a)
instance Serialise a => Serialise (HsImplicitBndrs GhcPs a)

deriving instance Generic (AmbiguousFieldOcc GhcPs)
instance Serialise (AmbiguousFieldOcc GhcPs)

deriving instance Generic (PatSynBind GhcPs GhcPs)
instance Serialise (PatSynBind GhcPs GhcPs)

deriving instance Generic (HsConDetails a b)
instance (Serialise a, Serialise b) => Serialise (HsConDetails a b)

deriving instance Generic (RecordPatSynField a)
instance Serialise a => Serialise (RecordPatSynField a)

deriving instance Generic (HsOverLit GhcPs)
instance Serialise (HsOverLit GhcPs)

deriving instance Generic OverLitVal
instance Serialise OverLitVal

deriving instance Generic IntegralLit
instance Serialise IntegralLit

deriving instance Generic FractionalLit
instance Serialise FractionalLit

deriving instance Generic (Ratio a)
instance Serialise a => Serialise (Ratio a)

instance Serialise SourceText where
  sput = binsput
  sget = binsget

deriving instance Generic (Pat GhcPs)
instance Serialise (Pat GhcPs) where
  sput bh pat = case pat of
    WildPat a -> do
      putTag bh 0
      sput bh a
    VarPat a b -> do
      putTag bh 1
      sput bh a >> sput bh b
    LazyPat a b -> do
      putTag bh 2
      sput bh a >> sput bh b
    AsPat a b c -> do
      putTag bh 3
      sput bh a >> sput bh b >> sput bh c
    ParPat a b -> do
      putTag bh 4
      sput bh a >> sput bh b
    BangPat a b -> do
      putTag bh 5
      sput bh a >> sput bh b
    ListPat a b -> do
      putTag bh 6
      sput bh a >> sput bh b
    TuplePat a b c -> do
      putTag bh 7
      sput bh a >> sput bh b >> sput bh c
    SumPat a b c d -> do
      putTag bh 8
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    ConPatIn a b -> do
      putTag bh 9
      sput bh a >> sput bh b
    ViewPat a b c -> do
      putTag bh 10
      sput bh a >> sput bh b >> sput bh c
    SplicePat a b -> do
      putTag bh 11
      sput bh a >> sput bh b
    LitPat a b -> do
      putTag bh 12
      sput bh a >> sput bh b
    NPat a b c d -> do
      putTag bh 13
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    NPlusKPat a b c d e f -> do
      putTag bh 14
      sput bh a >> sput bh b >> sput bh c >> sput bh d
      sput bh e >> sput bh f
    SigPat a b c -> do
      putTag bh 15
      sput bh a >> sput bh b >> sput bh c
    XPat a -> do
      putTag bh 16
      sput bh a

    CoPat {} -> unsupported "CoPat" "Pat" (ppr pat)
    ConPatOut {} -> unsupported "ConPatOut" "Pat" (ppr pat)

  sget bh = do
    tag <- getTag bh
    case tag of
      0 -> WildPat <$> sget bh
      1 -> VarPat <$> sget bh <*> sget bh
      2 -> LazyPat <$> sget bh <*> sget bh
      3 -> AsPat <$> sget bh <*> sget bh <*> sget bh
      4 -> ParPat <$> sget bh <*> sget bh
      5 -> BangPat <$> sget bh <*> sget bh
      6 -> ListPat <$> sget bh <*> sget bh
      7 -> TuplePat <$> sget bh <*> sget bh <*> sget bh
      8 -> SumPat <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      9 -> ConPatIn <$> sget bh <*> sget bh
      10 -> ViewPat <$> sget bh <*> sget bh <*> sget bh
      11 -> SplicePat <$> sget bh <*> sget bh
      12 -> LitPat <$> sget bh <*> sget bh
      13 -> NPat <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      14 -> NPlusKPat <$> sget bh <*> sget bh <*> sget bh
                      <*> sget bh <*> sget bh <*> sget bh
      15 -> SigPat <$> sget bh <*> sget bh <*> sget bh
      16 -> XPat <$> sget bh
      n -> badTag "Pat" n

deriving instance Generic SpliceDecoration
instance Serialise SpliceDecoration

deriving instance Generic (HsSplicedThing GhcPs)
instance Serialise (HsSplicedThing GhcPs)

deriving instance Generic (HsSplice GhcPs)
instance Serialise (HsSplice GhcPs) where
  sput bh s = case s of
    HsTypedSplice a b c d -> do
      putTag bh 0
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    HsUntypedSplice a b c d -> do
      putTag bh 1
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    HsQuasiQuote a b c d e -> do
      putTag bh 2
      sput bh a >> sput bh b >> sput bh c >> sput bh d >> sput bh e
    XSplice _ ->
      putTag bh 3
    HsSpliced {} -> unsupported "HsSpliced" "HsSplice" (ppr s)
    HsSplicedT {} -> unsupported "HsSplicedT" "HsSplice" (ppr s)
  sget bh = do
    tag <- getTag bh
    case tag of
      0 -> HsTypedSplice <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      1 -> HsUntypedSplice <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      2 -> HsQuasiQuote <$> sget bh <*> sget bh <*> sget bh <*> sget bh
                        <*> sget bh
      3 -> pure $ XSplice (error "sget @HsSplice: NoExtCon in XSplice")
      n -> badTag "HsSplice" n

instance Serialise IfaceType where
  sput = binsput
  sget = binsget

deriving instance Generic ForallVisFlag
instance Serialise ForallVisFlag

deriving instance Generic (HsType GhcPs)
instance Serialise (HsType GhcPs) where
  sput bh ty = case ty of
    HsForAllTy a b c d -> do
      putTag bh 0
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    HsQualTy a b c -> do
      putTag bh 1
      sput bh a >> sput bh b >> sput bh c
    HsTyVar a b c -> do
      putTag bh 2
      sput bh a >> sput bh b >> sput bh c
    HsAppTy a b c -> do
      putTag bh 3
      sput bh a >> sput bh b >> sput bh c
    HsAppKindTy a b c -> do
      putTag bh 4
      sput bh a >> sput bh b >> sput bh c
    HsFunTy a b c -> do
      putTag bh 5
      sput bh a >> sput bh b >> sput bh c
    HsListTy a b -> do
      putTag bh 6
      sput bh a >> sput bh b
    HsTupleTy a b c -> do
      putTag bh 7
      sput bh a >> sput bh b >> sput bh c
    HsSumTy a b -> do
      putTag bh 8
      sput bh a >> sput bh b
    HsOpTy a b c d -> do
      putTag bh 9
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    HsParTy a b -> do
      putTag bh 10
      sput bh a >> sput bh b
    HsIParamTy a b c -> do
      putTag bh 11
      sput bh a >> sput bh b >> sput bh c
    HsStarTy a b -> do
      putTag bh 12
      sput bh a >> sput bh b
    HsKindSig a b c -> do
      putTag bh 13
      sput bh a >> sput bh b >> sput bh c
    HsSpliceTy a b -> do
      putTag bh 14
      sput bh a >> sput bh b
    HsDocTy a b c -> do
      putTag bh 15
      sput bh a >> sput bh b >> sput bh c
    HsBangTy a b c -> do
      putTag bh 16
      sput bh a >> sput bh b >> sput bh c
    HsRecTy a b -> do
      putTag bh 17
      sput bh a >> sput bh b
    HsExplicitListTy a b c -> do
      putTag bh 18
      sput bh a >> sput bh b >> sput bh c
    HsExplicitTupleTy a b -> do
      putTag bh 19
      sput bh a >> sput bh b
    HsTyLit a b -> do
      putTag bh 20
      sput bh a >> sput bh b
    HsWildCardTy a -> do
      putTag bh 21
      sput bh a
    t@(XHsType _) -> unsupported "HsType" "XHsType" (ppr t)

  sget bh = do
    tag <- getTag bh
    case tag of
      0 -> HsForAllTy <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      1 -> HsQualTy <$> sget bh <*> sget bh <*> sget bh
      2 -> HsTyVar <$> sget bh <*> sget bh <*> sget bh
      3 -> HsAppTy <$> sget bh <*> sget bh <*> sget bh
      4 -> HsAppKindTy <$> sget bh <*> sget bh <*> sget bh
      5 -> HsFunTy <$> sget bh <*> sget bh <*> sget bh
      6 -> HsListTy <$> sget bh <*> sget bh
      7 -> HsTupleTy <$> sget bh <*> sget bh <*> sget bh
      8 -> HsSumTy <$> sget bh <*> sget bh
      9 -> HsOpTy <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      10 -> HsParTy <$> sget bh <*> sget bh
      11 -> HsIParamTy <$> sget bh <*> sget bh <*> sget bh
      12 -> HsStarTy <$> sget bh <*> sget bh
      13 -> HsKindSig <$> sget bh <*> sget bh <*> sget bh
      14 -> HsSpliceTy <$> sget bh <*> sget bh
      15 -> HsDocTy <$> sget bh <*> sget bh <*> sget bh
      16 -> HsBangTy <$> sget bh <*> sget bh <*> sget bh
      17 -> HsRecTy <$> sget bh <*> sget bh
      18 -> HsExplicitListTy <$> sget bh <*> sget bh <*> sget bh
      19 -> HsExplicitTupleTy <$> sget bh <*> sget bh
      20 -> HsTyLit <$> sget bh <*> sget bh
      21 -> HsWildCardTy <$> sget bh
      n  -> badTag "HsType" n

deriving instance Generic (HsTyVarBndr GhcPs)
instance Serialise (HsTyVarBndr GhcPs)

deriving instance Generic (ConDeclField GhcPs)
instance Serialise (ConDeclField GhcPs)

instance Serialise (HsLit GhcPs) where
  sput bh l = case l of
    HsChar a b -> putTag bh 0 >> sput bh a >> sput bh b
    HsCharPrim a b -> putTag bh 1 >> sput bh a >> sput bh b
    HsString a b -> putTag bh 2 >> sput bh a >> sput bh b
    HsStringPrim a b -> putTag bh 3 >> sput bh a >> sput bh b
    HsInt a b -> putTag bh 4 >> sput bh a >> sput bh b
    HsIntPrim a b -> putTag bh 5 >> sput bh a >> sput bh b
    HsWordPrim a b -> putTag bh 6 >> sput bh a >> sput bh b
    HsInt64Prim a b -> putTag bh 7 >> sput bh a >> sput bh b
    HsWord64Prim a b -> putTag bh 8 >> sput bh a >> sput bh b
    HsInteger a b ty -> do
      ty' :: IfaceType <- convertType ty
      putTag bh 9
      sput bh a >> sput bh b >> sput bh ty'
    HsRat a b ty -> do
      ty' :: IfaceType <- convertType ty
      putTag bh 10
      sput bh a >> sput bh b >> sput bh ty'
    HsFloatPrim a b -> putTag bh 11 >> sput bh a >> sput bh b
    HsDoublePrim a b -> putTag bh 12 >> sput bh a >> sput bh b
    XLit _ -> putTag bh 13

  sget bh = do
    tag <- getTag bh
    case tag of
      0 -> HsChar <$> sget bh <*> sget bh
      1 -> HsCharPrim <$> sget bh <*> sget bh
      2 -> HsString <$> sget bh <*> sget bh
      3 -> HsStringPrim <$> sget bh <*> sget bh
      4 -> HsInt <$> sget bh <*> sget bh
      5 -> HsIntPrim <$> sget bh <*> sget bh
      6 -> HsWordPrim <$> sget bh <*> sget bh
      7 -> HsInt64Prim <$> sget bh <*> sget bh
      8 -> HsWord64Prim <$> sget bh <*> sget bh
      9 -> do a <- sget bh
              b <- sget bh
              ty :: IfaceType <- sget bh
              ty' <- convertType ty
              return (HsInteger a b ty')
      10 -> do a <- sget bh
               b <- sget bh
               ty :: IfaceType <- sget bh
               ty' <- convertType ty
               return (HsRat a b ty')
      11 -> HsFloatPrim <$> sget bh <*> sget bh
      12 -> HsDoublePrim <$> sget bh <*> sget bh
      13 -> pure $ XLit (error "sget @HsLit: NoExtCon in XLit")
      n  -> badTag "HsLit" n

deriving instance Generic (HsPatSynDir GhcPs)
instance Serialise (HsPatSynDir GhcPs)

deriving instance Generic SrcUnpackedness
instance Serialise SrcUnpackedness

deriving instance Generic (HsTupArg GhcPs)
instance Serialise (HsTupArg GhcPs)

deriving instance Generic (ApplicativeArg GhcPs)
instance Serialise (ApplicativeArg GhcPs)

deriving instance Generic TransForm
instance Serialise TransForm

deriving instance Generic (ArithSeqInfo GhcPs)
instance Serialise (ArithSeqInfo GhcPs)

deriving instance Generic (Tickish a)
instance Serialise a => Serialise (Tickish a)

instance Serialise RealSrcSpan where
  sput = binsput
  sget = binsget

instance Serialise CostCentre where
  sput = binsput
  sget = binsget

deriving instance Generic (HsBracket GhcPs)
instance Serialise (HsBracket GhcPs)

deriving instance Generic (StandaloneKindSig GhcPs)
instance Serialise (StandaloneKindSig GhcPs)

deriving instance Generic (HsDecl GhcPs)
instance Serialise (HsDecl GhcPs)

instance Serialise (Sig GhcPs) where
  sput bh s = case s of
    TypeSig a b c -> do
      putTag bh 0
      sput bh a >> sput bh b >> sput bh c
    PatSynSig a b c -> do
      putTag bh 1
      sput bh a >> sput bh b >> sput bh c
    ClassOpSig a b c d -> do
      putTag bh 2
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    FixSig a b -> do
      putTag bh 3
      sput bh a >> sput bh b
    InlineSig a b c -> do
      putTag bh 4
      sput bh a >> sput bh b >> sput bh c
    SpecSig a b c d -> do
      putTag bh 5
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    SpecInstSig a b c -> do
      putTag bh 6
      sput bh a >> sput bh b >> sput bh c
    MinimalSig a b c -> do
      putTag bh 7
      sput bh a >> sput bh b >> sput bh c
    SCCFunSig a b c d -> do
      putTag bh 8
      sput bh a >> sput bh b >> sput bh c >> sput bh d
    CompleteMatchSig a b c d -> do
      putTag bh 9
      sput bh a >> sput bh b >> sput bh c >> sput bh d

    XSig {} -> unsupported "XSig" "IdSig" (ppr s)
    IdSig {} -> unsupported "Sig" "IdSig" (ppr s)

  sget bh = do
    tag <- getTag bh
    case tag of
      0 -> TypeSig <$> sget bh <*> sget bh <*> sget bh
      1 -> PatSynSig <$> sget bh <*> sget bh <*> sget bh
      2 -> ClassOpSig <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      3 -> FixSig <$> sget bh <*> sget bh
      4 -> InlineSig <$> sget bh <*> sget bh <*> sget bh
      5 -> SpecSig <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      6 -> SpecInstSig <$> sget bh <*> sget bh <*> sget bh
      7 -> MinimalSig <$> sget bh <*> sget bh <*> sget bh
      8 -> SCCFunSig <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      9 -> CompleteMatchSig <$> sget bh <*> sget bh <*> sget bh <*> sget bh
      n -> badTag "Sig" n

deriving instance Generic Fixity
instance Serialise Fixity

deriving instance Generic FixityDirection
instance Serialise FixityDirection

deriving instance Generic (FixitySig GhcPs)
instance Serialise (FixitySig GhcPs)

deriving instance Generic (BooleanFormula a)
instance Serialise a => Serialise (BooleanFormula a)

deriving instance Generic StringLiteral
instance Serialise StringLiteral

deriving instance Generic InlinePragma
instance Serialise InlinePragma

deriving instance Generic InlineSpec
instance Serialise InlineSpec

deriving instance Generic Activation
instance Serialise Activation

deriving instance Generic RuleMatchInfo
instance Serialise RuleMatchInfo

deriving instance Generic (TyClDecl GhcPs)
instance Serialise (TyClDecl GhcPs)

deriving instance Generic (FamEqn GhcPs a)
instance Serialise a => Serialise (FamEqn GhcPs a)

deriving instance Generic (FamilyDecl GhcPs)
instance Serialise (FamilyDecl GhcPs)

deriving instance Generic (FamilyInfo GhcPs)
instance Serialise (FamilyInfo GhcPs)

deriving instance Generic (HsArg a b)
instance (Serialise a, Serialise b) => Serialise (HsArg a b)

deriving instance Generic (LHsQTyVars GhcPs)
instance Serialise (LHsQTyVars GhcPs)

deriving instance Generic (FamilyResultSig GhcPs)
instance Serialise (FamilyResultSig GhcPs)

deriving instance Generic (InjectivityAnn GhcPs)
instance Serialise (InjectivityAnn GhcPs)

deriving instance Generic (HsDataDefn GhcPs)
instance Serialise (HsDataDefn GhcPs)

deriving instance Generic (ConDecl GhcPs)
instance Serialise (ConDecl GhcPs)

deriving instance Generic (HsDerivingClause GhcPs)
instance Serialise (HsDerivingClause GhcPs)

deriving instance Generic (DerivStrategy GhcPs)
instance Serialise (DerivStrategy GhcPs)

deriving instance Generic NewOrData
instance Serialise NewOrData

deriving instance Generic CType
instance Serialise CType

deriving instance Generic Header
instance Serialise Header

deriving instance Generic DocDecl
instance Serialise DocDecl

deriving instance Generic (InstDecl GhcPs)
instance Serialise (InstDecl GhcPs)

deriving instance Generic (ClsInstDecl GhcPs)
instance Serialise (ClsInstDecl GhcPs)

deriving instance Generic (TyFamInstDecl GhcPs)
instance Serialise (TyFamInstDecl GhcPs)

deriving instance Generic (DataFamInstDecl GhcPs)
instance Serialise (DataFamInstDecl GhcPs)

deriving instance Generic OverlapMode
instance Serialise OverlapMode

deriving instance Generic (DerivDecl GhcPs)
instance Serialise (DerivDecl GhcPs)

deriving instance Generic (DefaultDecl GhcPs)
instance Serialise (DefaultDecl GhcPs)

deriving instance Generic (ForeignDecl GhcPs)
instance Serialise (ForeignDecl GhcPs)

deriving instance Generic ForeignImport
instance Serialise ForeignImport

deriving instance Generic ForeignExport
instance Serialise ForeignExport

deriving instance Generic CCallConv
instance Serialise CCallConv

deriving instance Generic Safety
instance Serialise Safety

deriving instance Generic CImportSpec
instance Serialise CImportSpec

deriving instance Generic CExportSpec
instance Serialise CExportSpec

deriving instance Generic CCallTarget
instance Serialise CCallTarget

instance Serialise UnitId where
  sput = binsput
  sget = binsget

deriving instance Generic (WarnDecls GhcPs)
instance Serialise (WarnDecls GhcPs)

deriving instance Generic (WarnDecl GhcPs)
instance Serialise (WarnDecl GhcPs)

deriving instance Generic WarningTxt
instance Serialise WarningTxt

deriving instance Generic (AnnDecl GhcPs)
instance Serialise (AnnDecl GhcPs)

deriving instance Generic (AnnProvenance a)
instance Serialise a => Serialise (AnnProvenance a)

deriving instance Generic (RuleDecls GhcPs)
instance Serialise (RuleDecls GhcPs)

deriving instance Generic (RuleDecl GhcPs)
instance Serialise (RuleDecl GhcPs)

deriving instance Generic (RuleBndr GhcPs)
instance Serialise (RuleBndr GhcPs)

deriving instance Generic (SpliceDecl GhcPs)
instance Serialise (SpliceDecl GhcPs)

deriving instance Generic SpliceExplicitFlag
instance Serialise SpliceExplicitFlag

deriving instance Generic (RoleAnnotDecl GhcPs)
instance Serialise (RoleAnnotDecl GhcPs)

deriving instance Generic Role
instance Serialise Role

deriving instance Generic (HsGroup GhcPs)
instance Serialise (HsGroup GhcPs)

deriving instance Generic (TyClGroup GhcPs)
instance Serialise (TyClGroup GhcPs)

instance Serialise (HsCmd GhcPs) where
  sput bh cmd = case cmd of
    HsCmdArrApp a b c d e -> do
      putTag bh 0
      sput bh a >> sput bh b >> sput bh c >> sput bh d >> sput bh e
    HsCmdArrForm a b c d e -> do
      putTag bh 1
      sput bh a >> sput bh b >> sput bh c >> sput bh d >> sput bh e
    HsCmdApp a b c -> do
      putTag bh 2
      sput bh a >> sput bh b >> sput bh c
    HsCmdLam a b -> do
      putTag bh 3
      sput bh a >> sput bh b
    HsCmdPar a b -> do
      putTag bh 4
      sput bh a >> sput bh b
    HsCmdCase a b c -> do
      putTag bh 5
      sput bh a >> sput bh b >> sput bh c
    HsCmdIf a b c d e -> do
      putTag bh 6
      sput bh a >> sput bh b >> sput bh c >> sput bh d >> sput bh e
    HsCmdLet a b c -> do
      putTag bh 7
      sput bh a >> sput bh b >> sput bh c
    HsCmdDo a b -> do
      putTag bh 8
      sput bh a >> sput bh b
    XCmd a -> do
      putTag bh 9
      sput bh a
    HsCmdWrap {} -> unsupported "HsCmdWrap" "HsCmd" (ppr cmd)

  sget bh = do
    tag <- getTag bh
    case tag of
      0 -> HsCmdArrApp <$> sget bh <*> sget bh <*> sget bh
                       <*> sget bh <*> sget bh
      1 -> HsCmdArrForm <$> sget bh <*> sget bh <*> sget bh
                        <*> sget bh <*> sget bh
      2 -> HsCmdApp <$> sget bh <*> sget bh <*> sget bh
      3 -> HsCmdLam <$> sget bh <*> sget bh
      4 -> HsCmdPar <$> sget bh <*> sget bh
      5 -> HsCmdCase <$> sget bh <*> sget bh <*> sget bh
      6 -> HsCmdIf <$> sget bh <*> sget bh <*> sget bh
                   <*> sget bh <*> sget bh
      7 -> HsCmdLet <$> sget bh <*> sget bh <*> sget bh
      8 -> HsCmdDo <$> sget bh <*> sget bh
      9 -> XCmd <$> sget bh
      n -> badTag "HsCmd" n

deriving instance Generic (HsCmdTop GhcPs)
instance Serialise (HsCmdTop GhcPs)

deriving instance Generic HsArrAppType
instance Serialise HsArrAppType
