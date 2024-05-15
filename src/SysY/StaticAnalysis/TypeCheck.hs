{-# LANGUAGE 
    TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, 
    TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module SysY.StaticAnalysis.TypeCheck where
import SysY.AST
import Polysemy
import Text.Printf (printf)
import Control.Monad (unless, zipWithM)
import SysY.StaticAnalysis.Basic as SA
import Prelude hiding (error)

arith_op :: [Optr]
arith_op = [Plus, Minus, Mul, Div]
comp_op :: [Optr]
comp_op = [Lt, Gt, Le, Ge, Eq, Ne]
cond_op :: [Optr]
cond_op = [LAnd, LOr]

lift_typed :: (TermType, Exp) -> TypedExp
lift_typed (t, e) = (Just t, e)

typeInfer' :: Member SAEffects r => TypedExp -> Sem r TypedExp
typeInfer' e = do
    (t, e') <- typeInfer e
    pure (Just t, e')

findTypeDecl :: Member SAEffects r => Ident -> Sem r TermType
findTypeDecl = undefined

typeInfer :: Member SAEffects r => TypedExp -> Sem r (TermType, Exp)
typeInfer (Just t, e) = pure $ (t, e)
typeInfer (Nothing, ExpLVal (LVal name indexers)) = do
    carrier_type <- findTypeDecl name
    indexers_ <- mapM typeInfer' indexers
    indexers__ <- mapM (typeCheck False (TermBType BInt)) indexers_
    let val = LVal name indexers__
    let def = pure $ (TermAny, ExpLVal val)
    case carrier_type of
        TermAny -> def
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
        -- TermUArray type_ dimensions -> do
        --     if length dimensions + 1 < length indexers
        --         then do
        --             error "Indexers cannot be more than dimensions of array type"
        --             def
        --         else do
        --             if null indexers
        --                 then pure $ (carrier_type, ExpLVal val)
        --                 else do
        --                     let new_dimensions = drop (length indexers - 1) dimensions
        --                     if null new_dimensions
        --                         then pure $ (TermBType type_, ExpLVal val)
        --                         else do
        --                             let new_type = TermArray type_ new_dimensions
        --                             pure $ (new_type, ExpLVal val)

typeInfer (Nothing, ExpNum (IntConst i)) = pure $ (TermBType BInt, ExpNum (IntConst i))
typeInfer (Nothing, ExpNum (FloatConst f)) = pure $ (TermBType BFloat, ExpNum (FloatConst f))
typeInfer (Nothing, ExpOpUnary op oprd) = do
    (type_, oprd_) <- typeInfer oprd -- irrefutable
    let val t = pure $ (t, ExpOpUnary op (Just type_, oprd_))
    case type_ of
        TermAny -> val TermAny
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
        -- TermUArray _ _ -> do
        --     error $ printf "Operator '%s' cannot be used on array" (show op)
        --     val TermAny
typeInfer (Nothing, ExpOpBinary op lhs rhs) = do
    (ltype, lhs_) <- typeInfer lhs
    (rtype, rhs_) <- typeInfer rhs
    let val t = pure $ (t, ExpOpBinary op (Just ltype, lhs_) (Just rtype, rhs_))
    case (ltype, rtype) of
        (TermAny, _) -> val TermAny
        (_, TermAny) -> val TermAny
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
        -- (TermUArray _ _, _) -> do
        --         error $ printf "Operator '%s' cannot be used on array" (show op)
        --         val TermAny
        -- (_, TermUArray _ _) -> do
        --         error $ printf "Operator '%s' cannot be used on array" (show op)
        --         val TermAny
typeInfer (Nothing, ExpCall name args) = do
    func <- findFunc name
    args_ <- mapM typeInfer' args
    case func of
        Nothing -> do
            error $ printf "Function %s is not defined" name
            pure $ (TermAny, ExpCall name args_)
        Just (FuncInfo _ ret_type arg_types) -> do
            new_args <- if length args /= length arg_types
                then do
                    error "Number of function arguments mismatched"
                    pure args_
                -- only check types if number of arguments is right
                else zipWithM (typeCheck True) arg_types args_
            pure $ (ret_type, ExpCall name new_args)
        Just (LibFuncInfo _ ret_type arg_types) -> do
            undefined

typeCheck :: Member SAEffects r => Bool -> TermType -> TypedExp -> Sem r TypedExp
typeCheck implicit_conv t (type_, exp_) = do
    if type_ == Just t
        then pure (type_, exp_)
        else do
            o <- case type_ of
                Just type__ -> pure type__
                Nothing -> fst <$> typeInfer (type_, exp_)
            type__ <- if not implicit_conv || (o, t) `elem` typeImplicitConv
                then pure $ Just t
                else do
                    error $ printf "Expected type %s, got %s" (show t) (show o)
                    pure $ Just o -- won't change type if the conversion failed
            pure (type__, exp_)

typeImplicitConv :: [(TermType, TermType)]
typeImplicitConv = [(TermBType BInt, TermBType BFloat), (TermBType BFloat, TermBType BInt)]

-- interpretTypeCheckBySAEffects ::
--     Member SAEffects r =>
--         Sem (SAEffects ': r) a -> Sem r a
-- interpretTypeCheckBySAEffects = interpret \case
--     TypeError err -> SA.error err
--     FindFunction n -> findFunc n
--     FindTypeDecl n -> do
--         sym <- findSymbol n
--         case sym of
--             Nothing -> pure TermAny
--             Just (SymInfo _ t _, _) -> pure t

-- typeCheckInitVal :: Member SAEffects r => TermType -> ConstInitVal -> Sem r ()
-- typeCheckInitVal = undefined


