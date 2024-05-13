{-# LANGUAGE 
    TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, 
    TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
module SysY.StaticAnalysis.Basic where
import SysY.AST (Ident, TermType)
import Polysemy

-- data SysYBType = SyInt | SyFloat
--     deriving (Eq, Show)


data SymInfo = SymInfo Ident TermType
data FuncInfo = FuncInfo Ident TermType [TermType]

data SAEffects m a where
    Warn :: String -> SAEffects m ()
    Error :: String -> SAEffects m ()
    NewSymTable :: SAEffects m ()
    ExitSymTable :: SAEffects m ()
    NewFunc :: FuncInfo -> SAEffects m ()
    FindFunc :: Ident -> SAEffects m (Maybe FuncInfo)
    NewSymbol :: SymInfo -> SAEffects m ()
    FindSymbol :: Ident -> SAEffects m (Maybe SymInfo)
makeSem ''SAEffects

