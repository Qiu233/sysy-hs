{-# LANGUAGE 
    TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, 
    TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module SysY.StaticAnalysis.Basic (
    SAEffects, warn, SysY.StaticAnalysis.Basic.error, withScope, newFunc, findFunc, newSymbol, findSymbol,
    SymInfo(..), FuncInfo(..), SAContext(..), defaultSAContext, runSAEffectsPure, lib_functions
) where
import SysY.AST (Ident, TermType, ConstVal, TermType(..), BType(..))
import Polysemy
import Data.HashMap as Map
import Control.Monad.State
import Control.Lens

data SymInfo = SymInfo Ident TermType (Maybe ConstVal)
data FuncInfo
    = FuncInfo Ident TermType [TermType]
    | LibFuncInfo Ident TermType (Maybe [TermType])
    -- arg types of some lib functions must be checked separately

funcName :: FuncInfo -> Ident
funcName (FuncInfo name _ _) = name
funcName (LibFuncInfo name _ _) = name

data SAEffects m a where
    Warn :: String -> SAEffects m ()
    Error :: String -> SAEffects m ()
    NewScope :: SAEffects m () -- TODO: how about higher order effect?
    ExitScope :: SAEffects m ()
    NewFunc :: FuncInfo -> SAEffects m ()
    FindFunc :: Ident -> SAEffects m (Maybe FuncInfo)
    NewSymbol :: SymInfo -> SAEffects m ()
    FindSymbol :: Ident -> SAEffects m (Maybe (SymInfo, Bool))
makeSem ''SAEffects


data SAContext = SAContext
    { _warnings :: [String]
    , _errors :: [String]
    , _symbols :: [Map Ident SymInfo]
    , _functions :: Map Ident FuncInfo
    }
makeLenses ''SAContext

defaultSAContext :: SAContext
defaultSAContext = SAContext
    { _warnings = []
    , _errors = []
    , _symbols = [Map.empty] -- top level scope
    , _functions = Map.fromList lib_funcs
    }
    where lib_funcs = zip (fmap funcName lib_functions) lib_functions

append_warning :: String -> State SAContext ()
append_warning w = warnings %= (w <|) -- this version of lens does not have `<|=`
append_error :: String -> State SAContext ()
append_error e = errors %= (e <|)

new_scope :: State SAContext ()
new_scope = symbols %= (Map.empty <|)

exit_scope :: State SAContext ()
exit_scope = symbols %= tail

new_func :: FuncInfo -> State SAContext ()
new_func func = do
    functions %= Map.insert (funcName func) func
-- for unknown reason, lens of `Map` doesn't work, so I must manipulate it by hand

find_func :: Ident -> State SAContext (Maybe FuncInfo)
find_func name = Map.lookup name <$> use functions

new_symbol :: SymInfo -> State SAContext ()
new_symbol sym = do
    let (SymInfo name _ _) = sym
    symbols . _head %= Map.insert name sym

find_symbol :: Ident -> State SAContext (Maybe (SymInfo, Bool))
find_symbol name = do
    syms <- use symbols
    case syms of
        [] -> Prelude.error "impossible"
        m : rem_ -> case Map.lookup name m of
            Just t -> pure $ Just (t, True)
            Nothing -> case rem_ of
                [] -> pure Nothing
                _ -> pure $ go rem_
    where
        go :: [Map Ident SymInfo] -> Maybe (SymInfo, Bool)
        go [] = Prelude.error "impossible"
        go [m] = case Map.lookup name m of
            Nothing -> Nothing
            Just t -> Just (t, False)
        go (m : rem_) = case Map.lookup name m of
            Nothing -> go rem_
            Just t -> Just (t, False)

reinterpretSAEffectsByContext :: forall r a. Sem (SAEffects ': r) a -> Sem (Embed (State SAContext) : r) a
reinterpretSAEffectsByContext = reinterpret \case
    Warn w -> embed $ append_warning w
    Error e -> embed $ append_error e
    NewScope -> embed new_scope
    ExitScope -> embed exit_scope
    NewFunc f -> embed $ new_func f
    FindFunc name -> embed $ find_func name
    NewSymbol s -> embed $ new_symbol s
    FindSymbol name -> embed $ find_symbol name

withScope :: Member SAEffects r => Sem r a -> Sem r a
withScope p = do
    newScope
    a <- p
    exitScope
    pure a

runSAEffectsPure :: Sem '[SAEffects] a -> State SAContext a
runSAEffectsPure p = do
    runM $ reinterpretSAEffectsByContext @'[] p


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


