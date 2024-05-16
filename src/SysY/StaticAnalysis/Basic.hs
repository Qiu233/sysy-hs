{-# LANGUAGE 
    TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, 
    TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module SysY.StaticAnalysis.Basic (
    SAEffects(..), warn, SysY.StaticAnalysis.Basic.error, withScope, newFunc, findFunc, newSymbol, findSymbol,
    SymInfo(..), FuncInfo(..), resetSymbols, hasError, lib_functions
) where

import SysY.AST (Ident, TermType, ConstVal, TermType(..), BType(..))
import Polysemy

data SymInfo = SymInfo Ident TermType (Maybe ConstVal)
data FuncInfo
    = FuncInfo Ident TermType [TermType]
    | LibFuncInfo Ident TermType (Maybe [TermType])
    -- arg types of some lib functions must be checked separately

data SAEffects m a where
    Warn :: String -> SAEffects m ()
    Error :: String -> SAEffects m ()
    NewScope :: SAEffects m () -- TODO: how about higher order effect?
    ExitScope :: SAEffects m ()
    NewFunc :: FuncInfo -> SAEffects m ()
    FindFunc :: Ident -> SAEffects m (Maybe FuncInfo)
    NewSymbol :: SymInfo -> SAEffects m ()
    FindSymbol :: Ident -> SAEffects m (Maybe (SymInfo, Bool))
    ResetSymbols :: SAEffects m ()
    HasError :: SAEffects m Bool
makeSem ''SAEffects

withScope :: Member SAEffects r => Sem r a -> Sem r a
withScope p = do
    newScope
    a <- p
    exitScope
    pure a

lib_functions :: [FuncInfo]
lib_functions =
    [ LibFuncInfo "getint"    bint     (Just [])
    , LibFuncInfo "getch"     bint     (Just [])
    , LibFuncInfo "getarray"  bint     (Just [bintarr])
    , LibFuncInfo "putint"    TermVoid (Just [bint])
    , LibFuncInfo "putch"     TermVoid (Just [bint])
    , LibFuncInfo "putarray"  TermVoid (Just [bint, bintarr])
    , LibFuncInfo "putf"      TermVoid Nothing -- special case
    , LibFuncInfo "starttime" TermVoid (Just [])
    , LibFuncInfo "stoptime"  TermVoid (Just [])
    ]
    where
        bint = TermBType BInt
        bintarr = TermArray BInt [Nothing]
