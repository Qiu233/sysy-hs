{-# LANGUAGE 
    TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, 
    TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, TypeApplications #-}
module SysY.StaticAnalysis.Pure (SAContext (SAContext), runSAEffectsPure, defaultSAContext) where
import Data.HashMap as Map
import SysY.AST
import SysY.StaticAnalysis.Basic
import Control.Lens
import Control.Monad.State
import Polysemy

funcName :: FuncInfo -> Ident
funcName (FuncInfo name _ _) = name
funcName (LibFuncInfo name _ _) = name

data SAContext = SAContext
    { _warnings :: [String]
    , _errors :: [String]
    , _symbols :: [Map Ident SymInfo]
    , _functions :: Map Ident FuncInfo
    }
makeLenses ''SAContext

lib_funcs :: [(Ident, FuncInfo)]
lib_funcs = zip (fmap funcName lib_functions) lib_functions

defaultSAContext :: SAContext
defaultSAContext = SAContext
    { _warnings = []
    , _errors = []
    , _symbols = [Map.empty] -- top level scope
    , _functions = Map.fromList lib_funcs
    }

reset_symbols :: State SAContext ()
reset_symbols = do
    symbols .= [Map.empty]
    functions .= Map.fromList lib_funcs

has_error :: State SAContext Bool
has_error = uses errors (not . Prelude.null)

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
    ResetSymbols -> embed reset_symbols
    HasError -> embed has_error

runSAEffectsPure :: Sem '[SAEffects] a -> State SAContext a
runSAEffectsPure p = do
    runM $ reinterpretSAEffectsByContext @'[] p

