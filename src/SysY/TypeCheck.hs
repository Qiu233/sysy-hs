{-# LANGUAGE 
    TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, 
    TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module SysY.TypeCheck where
import SysY.AST
import Polysemy (makeSem, Member, Sem)
import Text.Printf (printf)
import Control.Monad (forM_, unless, zipWithM)


data SysYBType = SyInt | SyFloat
    deriving (Eq, Show)

data TermType
    -- when type inference fails or the type 
    -- doesn't matter in terms of type check
    = TermAny
    | TermBType SysYBType
    | TermArray SysYBType [Integer]
    -- the special type `type[][a][b]...`
    | TermUArray SysYBType [Integer]
    -- Function has no type in SysY
    --- | TermFunc SysYType [SysYType]
    deriving (Eq, Show)

data TypeEffects m a where
    FindIdent :: Ident -> TypeEffects m TermType
    FindFunction :: Ident -> TypeEffects m (Maybe (TermType, [TermType]))
    TypeError :: String -> TypeEffects m ()

makeSem ''TypeEffects
class TypeInfer a b where
    type_infer :: Member TypeEffects r => a -> Sem r b

instance (Traversable t, TypeInfer a b) => TypeInfer (t a) (t b) where
    type_infer = mapM type_infer

makeAnnoTypes "Typed" ''TermType [''Exp] ''TypeInfer 'type_infer

instance TypeInfer Exp TypedExp where
    type_infer = typeInfer

data TypeDecl = TypeDecl Ident TermType
    deriving (Eq, Show)

type TypeContext = [TypeDecl]

type_of_typed_exp :: TypedExp -> TermType
type_of_typed_exp (TypedExpLVal t _) = t
type_of_typed_exp (TypedExpNum t _) = t
type_of_typed_exp (TypedExpOpUnary t _ _) = t
type_of_typed_exp (TypedExpOpBinary t _ _ _) = t
type_of_typed_exp (TypedExpCall t _ _) = t

change_type_of_typed_exp :: TypedExp -> TermType -> TypedExp
change_type_of_typed_exp (TypedExpLVal _ x) t = TypedExpLVal t x
change_type_of_typed_exp (TypedExpNum _ x) t = TypedExpNum t x
change_type_of_typed_exp (TypedExpOpUnary _ op oprd) t = TypedExpOpUnary t op oprd
change_type_of_typed_exp (TypedExpOpBinary _ op lhs rhs) t = TypedExpOpBinary t op lhs rhs
change_type_of_typed_exp (TypedExpCall _ name args) t = TypedExpCall t name args

op2typed :: Optr -> TypedOptr
op2typed Plus = TypedPlus
op2typed Minus = TypedMinus
op2typed Flip = TypedFlip
op2typed Mul = TypedMul
op2typed Div = TypedDiv
op2typed Mod = TypedMod
op2typed Lt = TypedLt
op2typed Gt = TypedGt
op2typed Le = TypedLe
op2typed Ge = TypedGe
op2typed Eq = TypedEq
op2typed Ne = TypedNe
op2typed LAnd = TypedLAnd
op2typed LOr = TypedLOr

-- exp_any :: Exp -> TypedExp
-- exp_any (ExpLVal l) = TypedExpLVal TermAny l
-- exp_any (ExpNum n) = TypedExpNum TermAny n
-- exp_any (ExpOpUnary optr e) = TypedExpOpUnary TermAny optr (exp_any e)
-- exp_any (ExpOpBinary optr lhs rhs) = TypedExpOpBinary TermAny optr (exp_any lhs) (exp_any rhs)
-- exp_any (ExpCall name args) = TypedExpCall TermAny name (map exp_any args)



arith_op :: [Optr]
arith_op = [Plus, Minus, Mul, Div]
comp_op :: [Optr]
comp_op = [Lt, Gt, Le, Ge, Eq, Ne]
cond_op :: [Optr]
cond_op = [LAnd, LOr]

typeInfer :: Member TypeEffects r => Exp -> Sem r TypedExp
typeInfer (ExpLVal (LVal name indexers)) = do
    carrier_type <- findIdent name
    indexers_ <- mapM typeInfer indexers
    indexers__ <- mapM (typeCheck False (TermBType SyInt)) indexers_
    let val = TypedLVal name indexers__
    let def = pure $ TypedExpLVal TermAny val
    case carrier_type of
        TermAny -> def
        TermBType _ -> do
            typeError "Cannot index a non-array type"
            def
        TermArray type_ dimensions -> do
            if length dimensions < length indexers
                then do
                    typeError "Indexers cannot be more than dimensions of array type"
                    def
                else do
                    let new_dimensions = drop (length indexers) dimensions
                    if null new_dimensions
                        then pure $ TypedExpLVal (TermBType type_) val
                        else do
                            let new_type = TermArray type_ new_dimensions
                            pure $ TypedExpLVal new_type val
        TermUArray type_ dimensions -> do
            if length dimensions + 1 < length indexers
                then do
                    typeError "Indexers cannot be more than dimensions of array type"
                    def
                else do
                    if null indexers
                        then pure $ TypedExpLVal carrier_type val
                        else do
                            let new_dimensions = drop (length indexers - 1) dimensions
                            if null new_dimensions
                                then pure $ TypedExpLVal (TermBType type_) val
                                else do
                                    let new_type = TermArray type_ new_dimensions
                                    pure $ TypedExpLVal new_type val

typeInfer (ExpNum (IntConst i)) = pure $ TypedExpNum (TermBType SyInt) (TypedIntConst i)
typeInfer (ExpNum (FloatConst f)) = pure $ TypedExpNum (TermBType SyFloat) (TypedFloatConst f)
typeInfer (ExpOpUnary op oprd) = do
    oprd_ <- typeInfer oprd
    let val t = pure $ TypedExpOpUnary t (op2typed op) oprd_
    case type_of_typed_exp oprd_ of
        TermAny -> val TermAny
        TermBType SyInt -> val (TermBType SyInt)
        TermBType SyFloat ->
            if op == Flip
                then do
                    typeError $ printf "Operator '%s' cannot be used on float" (show op)
                    val TermAny
                else do
                    val (TermBType SyFloat)
        TermArray _ _ -> do
            typeError $ printf "Operator '%s' cannot be used on array" (show op)
            val TermAny
        TermUArray _ _ -> do
            typeError $ printf "Operator '%s' cannot be used on array" (show op)
            val TermAny
typeInfer (ExpOpBinary op lhs rhs) = do
    lhs_ <- typeInfer lhs
    rhs_ <- typeInfer rhs
    let val t = pure $ TypedExpOpBinary t (op2typed op) lhs_ rhs_
    case (type_of_typed_exp lhs_, type_of_typed_exp rhs_) of
        (TermAny, _) -> val TermAny
        (_, TermAny) -> val TermAny
        (TermBType SyInt, TermBType SyInt) -> val (TermBType SyInt)
        (TermBType SyFloat, TermBType SyFloat) -> do
            if op `elem` arith_op then
                val (TermBType SyFloat)
            else if op `elem` comp_op then
                val (TermBType SyInt)
            else do
                typeError $ printf "Operator '%s' cannot be used between float and float" (show op)
                val TermAny
        (TermBType SyInt, TermBType SyFloat) -> do
            if op `elem` arith_op then
                val (TermBType SyFloat)
            else if op `elem` comp_op then
                val (TermBType SyInt)
            else do
                typeError $ printf "Operator '%s' cannot be used between int and float" (show op)
                val TermAny
        (TermBType SyFloat, TermBType SyInt) -> do
            if op `elem` arith_op then
                val (TermBType SyFloat)
            else if op `elem` comp_op then
                val (TermBType SyInt)
            else do
                typeError $ printf "Operator '%s' cannot be used between float and int" (show op)
                val TermAny
        (TermArray _ _, _) -> do
                typeError $ printf "Operator '%s' cannot be used on array" (show op)
                val TermAny
        (_, TermArray _ _) -> do
                typeError $ printf "Operator '%s' cannot be used on array" (show op)
                val TermAny
        (TermUArray _ _, _) -> do
                typeError $ printf "Operator '%s' cannot be used on array" (show op)
                val TermAny
        (_, TermUArray _ _) -> do
                typeError $ printf "Operator '%s' cannot be used on array" (show op)
                val TermAny
typeInfer (ExpCall name args) = do
    func <- findFunction name
    args_ <- mapM typeInfer args
    -- let val t = pure $ TypedExpCall t name args_
    case func of
        Nothing -> do
            typeError $ printf "Function %s is not defined" name
            pure $ TypedExpCall TermAny name args_
        Just (ret_type, arg_types) -> do
            new_args <- if length args /= length arg_types
                then do
                    typeError "Number of function arguments mismatched"
                    pure args_
                -- only check types if number of arguments is right
                else zipWithM (typeCheck True) arg_types args_
            pure $ TypedExpCall ret_type name new_args

typeCheck :: Member TypeEffects r => Bool -> TermType -> TypedExp -> Sem r TypedExp
typeCheck implicit_conv t exp_ = do
    let o = type_of_typed_exp exp_
    if o == t
        then pure exp_
        else do
            let changed = change_type_of_typed_exp exp_ t
            unless (not implicit_conv || (o, t) `elem` typeImplicitConv) $ do
                typeError $ printf "Expected type %s, got %s" (show t) (show o)
            pure changed -- return changed so it won't cause too many errors

typeImplicitConv :: [(TermType, TermType)]
typeImplicitConv = [(TermBType SyInt, TermBType SyFloat), (TermBType SyFloat, TermBType SyInt)]

type_infer_comp_unit :: Member TypeEffects r => CompUnit -> Sem r TypedCompUnit
type_infer_comp_unit = type_infer

