{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module SysY.StaticAnalysis (
    module SysY.StaticAnalysis.Basic,
    module SysY.StaticAnalysis.TypeCheck
) where

import SysY.StaticAnalysis.Basic
import SysY.StaticAnalysis.TypeCheck
import Polysemy
import SysY.AST.Basic
import Control.Monad (forM, forM_, unless, join, zipWithM)
import Prelude hiding (error)
import Data.Maybe (catMaybes, mapMaybe, isJust, fromJust)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (lift)
import Data.Int
import Data.Bits (complement)
import GHC.Num (integerDiv)

-- -- | redirect error
-- sa_error :: Member SAEffects r => String -> Sem r ()
-- sa_error = SysY.StaticAnalysis.Basic.error

sa_typecheck_initval :: Member SAEffects r => TermType -> ConstInitVal -> Sem r ()
sa_typecheck_initval t v = interpretTypeCheckBySAEffects $ typeCheckInitVal t v
sa_exp :: Member SAEffects r => TypedExp -> Sem r TypedExp
sa_exp = interpretTypeCheckBySAEffects . typeInfer'
sa_exp_type :: Member SAEffects r => TermType -> TypedExp -> Sem r TypedExp
sa_exp_type t = interpretTypeCheckBySAEffects . typeCheck False t
sa_exp_type_implicit :: Member SAEffects r => TermType -> TypedExp -> Sem r TypedExp
sa_exp_type_implicit t = interpretTypeCheckBySAEffects . typeCheck True t

sa_block_item :: Member SAEffects r => BlockItem -> Sem r BlockItem
sa_block_item bi = do
    case bi of
        BlockItemStmt stmt -> BlockItemStmt <$> sa_stmt stmt
        BlockItemDecl (DeclConst d) -> BlockItemDecl . DeclConst <$> sa_const_decl d
        BlockItemDecl (DeclVar d) -> BlockItemDecl . DeclVar <$> sa_var_decl d

sa_var_decl :: Member SAEffects r => VarDecl -> Sem r VarDecl
sa_var_decl (VarDecl btype defs) = do
    VarDecl btype <$> forM defs (sa_var_def btype)

sa_var_def :: Member SAEffects r => BType -> VarDef -> Sem r VarDef
sa_var_def btype (VarDefUninit n dimensions) = do
    dimensions_ <- sa_indexers dimensions
    type_ <- sa_gen_type btype dimensions_
    newSymbol $ SymInfo n type_ Nothing
    pure $ VarDefUninit n dimensions_

sa_var_def btype (VarDefInit n dimensions init_val) = do
    dimensions_ <- sa_indexers dimensions -- check indexers
    type_ <- sa_gen_type btype dimensions_ -- generate array type
    init_val_ <- sa_initval init_val -- check the initial value
    sa_typecheck_initval type_ init_val_ -- check type of initial value
    newSymbol $ SymInfo n type_ Nothing -- add symbol to symbol table
    pure $ VarDefInit n dimensions_ init_val_

sa_const_decl :: Member SAEffects r => ConstDecl -> Sem r ConstDecl
sa_const_decl (ConstDecl btype defs) = do
    ConstDecl btype <$> forM defs (sa_const_def btype)

sa_const_def :: Member SAEffects r => BType -> ConstDef -> Sem r ConstDef
sa_const_def btype (ConstDef n dimensions init_val) = do
    dimensions_ <- sa_indexers dimensions
    type_ <- sa_gen_type btype dimensions_
    init_val_ <- sa_initval init_val
    unless (hasConst init_val_) $ error "Expected compile-time constant"
    sa_typecheck_initval type_ init_val_ -- check type
    -- TODO: reify the array literal
    newSymbol $ SymInfo n type_ (constVal init_val_) -- record the const value
    pure $ ConstDef n dimensions_ init_val_

-- -- | Pad and drop elements of array literal according to type
-- reify_array_const :: Member SAEffects r => TermType -> ConstVal -> Sem r (Maybe ConstVal)
-- reify_array_const (TermArray btype dimensions) v = do
--     let type_correct = not (null dimensions) && all isJust dimensions
--     if not type_correct
--         then pure Nothing -- failed
--         else undefined
-- reify_array_const _ _ = undefined

-- | Eagerly compute the const initial value and annotate it to AST node.
sa_initval :: Member SAEffects r => ConstInitVal -> Sem r ConstInitVal
sa_initval (_, InitValExp e) = do
    e' <- sa_exp e
    c <- sa_compute_const e' -- compute const value at compile-time, TODO: can this be vicious?
    pure $ (ConstInfo c, InitValExp e')
sa_initval (_, InitValArray is) = do -- array literal
    is' <- mapM sa_initval is
    let is_const = all hasConst is' -- TODO: locate errors
    let arr = mapMaybe constVal is'
    pure $ (ConstInfo (if is_const then Just (ConstValArray arr) else Nothing), InitValArray is')

-- | Check that indexers are of type `int`. Inferred type annotation won't be converted implicitly.
sa_indexers :: Member SAEffects r => [TypedExp] -> Sem r [TypedExp]
sa_indexers = mapM (sa_exp_type (TermBType BInt))

sa_dimensions_const :: Member SAEffects r => [TypedExp] -> Sem r [Maybe Integer]
sa_dimensions_const indexers = forM indexers $ \x -> do
    c <- sa_compute_const_int x -- check for Integer constant
    case c of
        Nothing -> do
            error "Dimensions of array declaration must be non-negative integer constant"
            pure Nothing
        Just t -> do
            if t < 0
                then do
                    error "Dimensions of array declaration must be non-negative integer constant"
                    pure Nothing
                else pure $ Just t

sa_gen_type :: Member SAEffects r => BType -> [TypedExp] -> Sem r TermType
sa_gen_type btype indexers = do
    indexers_ <- sa_dimensions_const indexers
    pure $ if null indexers
        then TermBType btype
        else TermArray btype indexers_

withSymbol :: Member SAEffects r => Ident -> a -> (SymInfo -> Sem r a) -> Sem r a
withSymbol name v with = do
    sym <- findSymbol name
    case sym of
        Nothing -> do
            error $ "No such symbol: " ++ name
            pure v
        Just i -> with i

-- | If non-implicit type checking succeeds for a `TypedExp`, and if it's compile-time constant,
-- then this function should always return computed value of it.
-- Since array literals are not `Exp`, this function won't in any way return an array constant.
-- Array constant is generated by function `sa_initval`, for the variant of `InitValArray`.
sa_compute_const :: Member SAEffects r => TypedExp -> Sem r (Maybe ConstVal)
sa_compute_const (_, ExpLVal (LVal name indexers)) = do
    withSymbol name Nothing $ \(SymInfo _ arr_type const_val) -> do
        case const_val of
            Nothing -> do
                error "Constant computation failed due to non-constant symbol"
                pure Nothing
            Just const_val_ -> do
                if null indexers
                    then pure $ Just const_val_
                    else do
                        indexers_ <- sa_indexers indexers
                        let type_correct = all ((\x -> x == Just (TermBType BInt)) . fst) indexers_
                        if not type_correct
                            then do
                                error "Constant computation failed due to type checking on indexers"
                                pure Nothing
                            else do
                                indexers_const <- mapM sa_compute_const_int indexers_
                                if not $ all isJust indexers_const
                                    then do
                                        error "Constant computation failed due to non-constant indexers"
                                        pure Nothing
                                    else do
                                        let indexers_const_ = map fromJust indexers_const
                                        sa_compute_const_array_indexing arr_type const_val_ indexers_const_
sa_compute_const (_, ExpNum num) = do
    case num of
        IntConst i -> pure $ Just $ ConstValInt i
        FloatConst f -> pure $ Just $ ConstValFloat f
sa_compute_const (_, ExpOpUnary op oprd) = runMaybeT $ do
    v <- MaybeT $ sa_compute_const oprd
    case (op, v) of
        (_, ConstValArray _) -> do
            lift $ error "Operator cannot be used on array"
            fail ""
        (Plus, ConstValInt _) -> pure v
        (Plus, ConstValFloat _) -> pure v
        (Minus, ConstValInt i) -> pure $ ConstValInt (-i)
        (Minus, ConstValFloat f) -> pure $ ConstValFloat (-f)
        (Flip, ConstValInt i) -> pure $ ConstValInt (sa_flip i)
        (_, _) -> do
            lift $ error "Unexpected unary operator"
            fail ""
    where
        (sa_flip) i = do
            let i' = fromIntegral i :: Int32
            fromIntegral $ complement i'
sa_compute_const (_, ExpOpBinary op lhs rhs) = runMaybeT $ do
    lhs_ <- MaybeT $ sa_compute_const lhs
    rhs_ <- MaybeT $ sa_compute_const rhs
    case (op, lhs_, rhs_) of
        (_, ConstValArray _, _) -> do
            lift $ error "Operator cannot be used on array"
            fail ""
        (_, _, ConstValArray _) -> do
            lift $ error "Operator cannot be used on array"
            fail ""
        (Plus, ConstValInt a, ConstValInt b) -> pure $ ConstValInt $ a + b
        (Plus, ConstValFloat a, ConstValFloat b) -> pure $ ConstValFloat $ a + b
        (Plus, ConstValInt a, ConstValFloat b) -> pure $ ConstValFloat $ fromIntegral a + b
        (Plus, ConstValFloat a, ConstValInt b) -> pure $ ConstValFloat $ a + fromIntegral b
        (Minus, ConstValInt a, ConstValInt b) -> pure $ ConstValInt $ a - b
        (Minus, ConstValFloat a, ConstValFloat b) -> pure $ ConstValFloat $ a - b
        (Minus, ConstValInt a, ConstValFloat b) -> pure $ ConstValFloat $ fromIntegral a - b
        (Minus, ConstValFloat a, ConstValInt b) -> pure $ ConstValFloat $ a - fromIntegral b
        (Mul, ConstValInt a, ConstValInt b) -> pure $ ConstValInt $ a * b
        (Mul, ConstValFloat a, ConstValFloat b) -> pure $ ConstValFloat $ a * b
        (Mul, ConstValInt a, ConstValFloat b) -> pure $ ConstValFloat $ fromIntegral a * b
        (Mul, ConstValFloat a, ConstValInt b) -> pure $ ConstValFloat $ a * fromIntegral b
        (Div, ConstValInt a, ConstValInt b) -> pure $ ConstValInt $ div a b
        (Div, ConstValFloat a, ConstValFloat b) -> pure $ ConstValFloat $ a / b
        (Div, ConstValInt a, ConstValFloat b) -> pure $ ConstValFloat $ fromIntegral a / b
        (Div, ConstValFloat a, ConstValInt b) -> pure $ ConstValFloat $ a / fromIntegral b
        (Mod, ConstValInt a, ConstValInt b) -> pure $ ConstValInt $ rem a b
        (LAnd, ConstValInt a, ConstValInt b) -> pure $ ConstValInt $ a %&& b
        (LOr, ConstValInt a, ConstValInt b) -> pure $ ConstValInt $ a %|| b
        (Eq, ConstValInt a, ConstValInt b) -> pure $ ConstValInt $ a %== b
        (Eq, ConstValFloat a, ConstValFloat b) -> pure $ ConstValInt $ a %== b
        (Eq, ConstValInt a, ConstValFloat b) -> pure $ ConstValInt $ fromIntegral a %== b
        (Eq, ConstValFloat a, ConstValInt b) -> pure $ ConstValInt $ a %== fromIntegral b
        (Ne, ConstValInt a, ConstValInt b) ->       pure $ ConstValInt $                a %!= b
        (Ne, ConstValFloat a, ConstValFloat b) ->   pure $ ConstValInt $                a %!= b
        (Ne, ConstValInt a, ConstValFloat b) ->     pure $ ConstValInt $ fromIntegral   a %!= b
        (Ne, ConstValFloat a, ConstValInt b) ->     pure $ ConstValInt $                a %!= fromIntegral b
        (Lt, ConstValInt a, ConstValInt b) ->       pure $ ConstValInt $                a %< b
        (Lt, ConstValFloat a, ConstValFloat b) ->   pure $ ConstValInt $                a %< b
        (Lt, ConstValInt a, ConstValFloat b) ->     pure $ ConstValInt $ fromIntegral   a %< b
        (Lt, ConstValFloat a, ConstValInt b) ->     pure $ ConstValInt $                a %< fromIntegral b
        (Gt, ConstValInt a, ConstValInt b) ->       pure $ ConstValInt $                a %> b
        (Gt, ConstValFloat a, ConstValFloat b) ->   pure $ ConstValInt $                a %> b
        (Gt, ConstValInt a, ConstValFloat b) ->     pure $ ConstValInt $ fromIntegral   a %> b
        (Gt, ConstValFloat a, ConstValInt b) ->     pure $ ConstValInt $                a %> fromIntegral b
        (Le, ConstValInt a, ConstValInt b) ->       pure $ ConstValInt $                a %<= b
        (Le, ConstValFloat a, ConstValFloat b) ->   pure $ ConstValInt $                a %<= b
        (Le, ConstValInt a, ConstValFloat b) ->     pure $ ConstValInt $ fromIntegral   a %<= b
        (Le, ConstValFloat a, ConstValInt b) ->     pure $ ConstValInt $                a %<= fromIntegral b
        (Ge, ConstValInt a, ConstValInt b) ->       pure $ ConstValInt $                a %>= b
        (Ge, ConstValFloat a, ConstValFloat b) ->   pure $ ConstValInt $                a %>= b
        (Ge, ConstValInt a, ConstValFloat b) ->     pure $ ConstValInt $ fromIntegral   a %>= b
        (Ge, ConstValFloat a, ConstValInt b) ->     pure $ ConstValInt $                a %>= fromIntegral b
        _ -> do
            lift $ error "Constant computation failed due to type error"
            fail ""
    where
        (%==) a b = if a == b then 1 else 0
        (%!=) a b = if a == b then 0 else 1
        (%<) a b = if a < b then 1 else 0
        (%>) a b = if a > b then 1 else 0
        (%<=) a b = if a <= b then 1 else 0
        (%>=) a b = if a >= b then 1 else 0
        (%&&) a b = if a /= 0 && b /= 0 then 1 else 0
        (%||) a b = if a /= 0 || b /= 0 then 1 else 0
sa_compute_const (_, ExpCall _ _) = do
    error "Does not support constant function"
    pure Nothing

sa_compute_const_array_indexing :: Member SAEffects r => TermType -> ConstVal -> [Integer] -> Sem r (Maybe ConstVal)
sa_compute_const_array_indexing arr_type init_val indexers = do
    case arr_type of
        TermArray btype dimensions -> do
            if not $ all isJust dimensions
                then do
                    error "Constant computation faild due to failure to determine dimensions of array"
                    pure Nothing
                else if length dimensions /= length indexers
                    then do
                        error "Constant computation of array must be fully indexed"
                        pure Nothing
                    else do
                        let dimensions_ = catMaybes dimensions
                        bounds_check <- and <$> zipWithM (\x y -> do
                            let r = x <= y
                            unless r (error "Constant computation of array failed due to index out of bounds")
                            pure r) indexers dimensions_
                        if not bounds_check
                            then pure Nothing
                            else undefined
        _ -> do
            error "Expected compile time constant array"
            pure Nothing

sa_compute_const_int :: Member SAEffects r => TypedExp -> Sem r (Maybe Integer)
sa_compute_const_int e = do
    c <- sa_compute_const e
    pure $ (go =<< c)
    where
        go (ConstValInt i) = Just i
        go _ = Nothing

sa_stmt :: Member SAEffects r => Stmt -> Sem r Stmt
sa_stmt (StmtLVal lval e) = do
    -- TODO: check that right side is fully indexed if it's array
    -- TODO: check that left side is valid symbol or fully indexed array
    undefined


sa_stmt (StmtExp Nothing) = pure (StmtExp Nothing)
sa_stmt (StmtExp (Just e)) = StmtExp . Just <$> sa_exp e
sa_stmt (StmtBlock (Block items)) = do
    items' <- mapM sa_block_item items
    pure $ StmtBlock (Block items')
sa_stmt (StmtIf cond then_ else_) = do
    cond' <- sa_exp cond
    then' <- sa_stmt then_
    else' <- join <$> case else_ of
        Nothing -> pure Nothing
        Just t -> pure . Just <$> sa_stmt t
    pure (StmtIf cond' then' else')
sa_stmt (StmtWhile cond do_) = do
    cond' <- sa_exp cond
    do' <- sa_stmt do_
    pure (StmtWhile cond' do')
sa_stmt StmtBreak = pure StmtBreak
sa_stmt StmtContinue = pure StmtContinue
sa_stmt (StmtReturn retval) = do
    retval' <- join <$> case retval of
        Nothing -> pure Nothing
        Just t -> pure . Just <$> sa_exp t
    pure $ StmtReturn retval'

