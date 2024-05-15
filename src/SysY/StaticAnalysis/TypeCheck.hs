{-# LANGUAGE 
    TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, 
    TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module SysY.StaticAnalysis.TypeCheck (

) where
import SysY.AST
import Polysemy
import Text.Printf (printf)
import Control.Monad (unless, zipWithM)
import SysY.StaticAnalysis.Basic as SA
import Prelude hiding (error)
import qualified Prelude as Pre

arith_op :: [Optr]
arith_op = [Plus, Minus, Mul, Div]
comp_op :: [Optr]
comp_op = [Lt, Gt, Le, Ge, Eq, Ne]
cond_op :: [Optr]
cond_op = [LAnd, LOr]

-- lift_typed :: (TermType, Exp) -> TypedExp
-- lift_typed (t, e) = (Just t, e)

typeInfer' :: Member SAEffects r => TypedExp -> Sem r (TermType, TypedExp)
typeInfer' (RawExp e) = do
    (t, e') <- typeInfer e
    pure (t, TypedExp t e')
typeInfer' (TypedExp t e) = pure (t, TypedExp t e)
typeInfer' (ConvExp t e) = pure (t, ConvExp t e)

typeInfer_ :: Member SAEffects r => TypedExp -> Sem r TypedExp
typeInfer_ e = snd <$> typeInfer' e

-- | when this error occurs, there must be logic error in compiler code
assert_name_resolution :: a
assert_name_resolution = Pre.error "Impossible. Name resolution should have failed before type checking."

findTypeDecl :: Member SAEffects r => Ident -> Sem r TermType
findTypeDecl name = do
    findSymbol name >>= \case
        -- type checking is designed to happen after name resolution, so abcense of name is impossible
        Nothing -> assert_name_resolution
        Just (SymInfo _ t _, _) -> pure t


-- | We assert that name resolution has succeeded, so type checking can safely panic when there're unexpected cases,
-- so in which cases compiler code should checked.
--
-- This functions cannot return `TermVoid`.
-- 
-- `TermAny` being returned indicates type inference has failed.
-- But caller should not generate error message in this case. The inference which failed is responsible to do that.
typeInfer :: Member SAEffects r => Exp -> Sem r (TermType, Exp)
typeInfer (ExpLVal (LVal name indexers)) = do
    carrier_type <- findTypeDecl name
    if null indexers then pure (carrier_type, ExpLVal (LVal name indexers))
    else do
        indexers_ <- mapM typeInfer_ indexers
        indexers__ <- mapM (typeCheck_ex (TermBType BInt)) indexers_
        let val = LVal name indexers__
        let def = pure $ (TermAny, ExpLVal val)
        case carrier_type of
            TermAny -> assert_name_resolution
            TermVoid -> Pre.error "Impossible. Symbol cannot be of void"
            TermBType _ -> do
                error "Cannot index a non-array type"
                def
            TermArray type_ dimensions -> do
                if length dimensions < length indexers
                    then do
                        error "Indexers cannot be more than dimensions of array type"
                        def
                    else do
                        let new_dimensions = drop (length indexers) dimensions
                        if null new_dimensions
                            then pure $ (TermBType type_, ExpLVal val)
                            else do
                                let new_type = TermArray type_ new_dimensions
                                pure $ (new_type, ExpLVal val)

typeInfer ((ExpNum (IntConst i))) = pure $ (TermBType BInt, ExpNum (IntConst i))
typeInfer ((ExpNum (FloatConst f))) = pure $ (TermBType BFloat, ExpNum (FloatConst f))
typeInfer ((ExpOpUnary op oprd)) = do
    (type_, oprd_) <- typeInfer' oprd -- irrefutable
    let val t = pure $ (t, ExpOpUnary op oprd_)
    case type_ of
        TermAny -> val TermAny
        TermVoid -> error "Unary operator cannot be applied to expression of void" >> val TermAny
        TermBType BInt -> val (TermBType BInt)
        TermBType BFloat ->
            if op == Flip
                then do
                    error $ printf "Operator '%s' cannot be used on float" (show op)
                    val TermAny
                else do
                    val (TermBType BFloat)
        TermArray _ _ -> do
            error $ printf "Operator '%s' cannot be used on array" (show op)
            val TermAny
typeInfer ((ExpOpBinary op lhs rhs)) = do
    (ltype, lhs_) <- typeInfer' lhs
    (rtype, rhs_) <- typeInfer' rhs
    let val t = pure $ (t, ExpOpBinary op lhs_ rhs_)
    case (ltype, rtype) of
        (TermAny, _) -> val TermAny
        (_, TermAny) -> val TermAny
        (TermVoid, _) -> do
            error $ printf "Operator %s cannot be applied to lhs expression of void" (show op)
            val TermAny
        (_, TermVoid) -> do
            error $ printf "Operator %s cannot be applied to rhs expression of void" (show op)
            val TermAny
        (TermBType BInt, TermBType BInt) -> val (TermBType BInt)
        (TermBType BFloat, TermBType BFloat) -> do
            if op `elem` arith_op then
                val (TermBType BFloat)
            else if op `elem` comp_op then
                val (TermBType BInt)
            else do
                error $ printf "Operator '%s' cannot be used between float and float" (show op)
                val TermAny
        (TermBType BInt, TermBType BFloat) -> do
            if op `elem` arith_op then
                val (TermBType BFloat)
            else if op `elem` comp_op then
                val (TermBType BInt)
            else do
                error $ printf "Operator '%s' cannot be used between int and float" (show op)
                val TermAny
        (TermBType BFloat, TermBType BInt) -> do
            if op `elem` arith_op then
                val (TermBType BFloat)
            else if op `elem` comp_op then
                val (TermBType BInt)
            else do
                error $ printf "Operator '%s' cannot be used between float and int" (show op)
                val TermAny
        (TermArray _ _, _) -> do
                error $ printf "Operator '%s' cannot be used on array" (show op)
                val TermAny
        (_, TermArray _ _) -> do
                error $ printf "Operator '%s' cannot be used on array" (show op)
                val TermAny
typeInfer ((ExpCall name args)) = do
    func <- findFunc name
    args_ <- mapM typeInfer_ args
    let check x = zipWithM typeCheck_im x args_
    case func of
        Nothing -> do
            error $ printf "Function %s is not defined" name
            pure $ (TermAny, ExpCall name args_)
        Just (FuncInfo _ ret_type arg_types) -> do
            new_args <- if length args_ /= length arg_types
                then do
                    error "Number of function arguments mismatched"
                    pure args_
                -- only check types if number of arguments is right
                else check arg_types
            pure $ (ret_type, ExpCall name new_args)
        Just (LibFuncInfo _ _ Nothing) -> undefined -- TODO: special cases like `putf`
        Just (LibFuncInfo _ ret_type (Just arg_types)) -> do
            new_args <- if length args_ /= length arg_types
                then do
                    error "Number of function arguments mismatched"
                    pure args_
                -- only check types if number of arguments is right
                else check arg_types
            pure $ (ret_type, ExpCall name new_args)


typeCheck_im :: Member SAEffects r => TermType -> TypedExp -> Sem r TypedExp
typeCheck_im = typeCheck True
typeCheck_ex :: Member SAEffects r => TermType -> TypedExp -> Sem r TypedExp
typeCheck_ex = typeCheck False

typeCheck :: Member SAEffects r => Bool -> TermType -> TypedExp -> Sem r TypedExp
typeCheck implicit_conv t (RawExp e) = do
    (type_, e_) <- typeInfer e
    typeCheck implicit_conv t (TypedExp type_ e_)
typeCheck implicit_conv t (TypedExp o e) = do -- combine the two cases
    if implicit_conv then
        if (o, t) `elem` typeImplicitConv then
            pure $ ConvExp t (TypedExp o e)
        else do
            error $ printf "Expected type %s, got %s, conversion failed" (show t) (show o)
            pure (TypedExp o e)
    else do
        unless (t == o) $ error $ printf "Expected type %s, got %s" (show t) (show o)
        pure (TypedExp o e)
typeCheck implicit_conv t (ConvExp o e) = do
    if implicit_conv then
        if (o, t) `elem` typeImplicitConv then
            pure $ ConvExp t (ConvExp o e) -- in the case where multiple conversion is needed
        else do
            error $ printf "Expected type %s, got %s, conversion failed" (show t) (show o)
            pure (ConvExp o e)
    else do
        unless (t == o) $ error $ printf "Expected type %s, got %s" (show t) (show o)
        pure (ConvExp o e)

typeImplicitConv :: [(TermType, TermType)]
typeImplicitConv = [(TermBType BInt, TermBType BFloat), (TermBType BFloat, TermBType BInt)]


-- check_typed_exp :: 

