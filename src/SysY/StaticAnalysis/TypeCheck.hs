{-# LANGUAGE 
    TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, 
    TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module SysY.StaticAnalysis.TypeCheck (
    check_comp_unit
) where
import SysY.AST
import Polysemy
import Text.Printf (printf)
import Control.Monad (unless, zipWithM, when)
import SysY.StaticAnalysis.Basic as SA
import Prelude hiding (error)
import qualified Prelude as Pre
import Data.Functor (($>))

arith_op :: [Optr]
arith_op = [Plus, Minus, Mul, Div]
comp_op :: [Optr]
comp_op = [Lt, Gt, Le, Ge, Eq, Ne]
-- cond_op :: [Optr]
-- cond_op = [LAnd, LOr]

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

check_lval :: Member SAEffects r => LVal -> Sem r (TermType, LVal)
check_lval (LVal name indexers) = do
    carrier_type <- findTypeDecl name
    if null indexers then pure (carrier_type, LVal name indexers)
    else do
        indexers' <- check_indexers indexers
        let val = LVal name indexers'
        let def = pure (TermAny, val)
        case carrier_type of
            TermAny -> assert_name_resolution
            TermVoid -> Pre.error "Impossible. Symbol cannot be of void"
            TermBType _ -> do
                error "Cannot index a non-array type"
                def
            TermArray type_ dimensions -> do
                if length dimensions < length indexers
                    then do
                        error "Indexers cannot be of more dimensions than of array type"
                        def
                    else do
                        let new_dimensions = drop (length indexers) dimensions
                        if null new_dimensions
                            then pure $ (TermBType type_, val)
                            else do
                                let new_type = TermArray type_ new_dimensions
                                pure $ (new_type, val)

-- | We assert that name resolution has succeeded, so type checking can safely panic when there're unexpected cases,
-- so in which cases compiler code should checked.
--
-- This functions cannot return `TermVoid`.
-- 
-- `TermAny` being returned indicates type inference has failed.
-- But caller should not generate error message in this case. The inference which failed is responsible to do that.
typeInfer :: Member SAEffects r => Exp -> Sem r (TermType, Exp)
typeInfer (ExpLVal lval) = do
    (type_, lval') <- check_lval lval
    pure (type_, ExpLVal lval')
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


check_indexers :: Member SAEffects r => [TypedExp] -> Sem r [TypedExp]
check_indexers = mapM (typeCheck_ex (TermBType BInt))

typeCheck_im :: Member SAEffects r => TermType -> TypedExp -> Sem r TypedExp
typeCheck_im = typeCheck True
typeCheck_ex :: Member SAEffects r => TermType -> TypedExp -> Sem r TypedExp
typeCheck_ex = typeCheck False

typeCheck :: Member SAEffects r => Bool -> TermType -> TypedExp -> Sem r TypedExp
typeCheck implicit_conv t (RawExp e) = do
    (type_, e_) <- typeInfer e
    typeCheck implicit_conv t (TypedExp type_ e_)
typeCheck implicit_conv t (TypedExp o e) = do -- combine the two cases
    if t == o then
        pure (TypedExp o e)
    else
        if implicit_conv then
            if (o, t) `elem` typeImplicitConv then
                pure $ ConvExp t (TypedExp o e)
            else do
                error $ printf "Expected type %s, got %s, conversion failed" (show t) (show o)
                pure (TypedExp o e)
        else do
            error $ printf "Expected type %s, got %s" (show t) (show o)
            pure (TypedExp o e)
typeCheck implicit_conv t (ConvExp o e) = do
    if t == o then
        pure (ConvExp o e)
    else
        if implicit_conv then
            if (o, t) `elem` typeImplicitConv then
                pure $ ConvExp t (ConvExp o e) -- in the case where multiple conversion is needed
            else do
                error $ printf "Expected type %s, got %s, conversion failed" (show t) (show o)
                pure (ConvExp o e)
        else do
            error $ printf "Expected type %s, got %s" (show t) (show o)
            pure (ConvExp o e)

typeImplicitConv :: [(TermType, TermType)]
typeImplicitConv = [(TermBType BInt, TermBType BFloat), (TermBType BFloat, TermBType BInt)]

check_typed_exp :: Member SAEffects r => TypedExp -> Sem r TypedExp
check_typed_exp = typeInfer_

check_decl :: Member SAEffects r => Decl -> Sem r Decl
check_decl (DeclConst (ConstDecl btype defs)) = do
    defs_ <- mapM (check_const_def btype) defs
    pure $ DeclConst (ConstDecl btype defs_)
check_decl (DeclVar (VarDecl btype defs)) = do
    defs_ <- mapM (check_var_def btype) defs
    pure $ DeclVar (VarDecl btype defs_)

check_const_def :: Member SAEffects r => BType -> ConstDef -> Sem r ConstDef
check_const_def btype (ConstDef name indexers init_) = do
    indexers' <- check_indexers indexers
    let type' = gen_type btype (length indexers')
    init' <- check_const_init_val type' init_ -- check type of init value with unknown dimensions length
    newSymbol (SymInfo name type' Nothing)
    pure (ConstDef name indexers' init')

check_var_def :: Member SAEffects r => BType -> VarDef -> Sem r VarDef
check_var_def btype (VarDefUninit name indexers) = do
    indexers' <- check_indexers indexers
    let type' = gen_type btype (length indexers')
    newSymbol (SymInfo name type' Nothing)
    pure (VarDefUninit name indexers')
check_var_def btype (VarDefInit name indexers init_) = do
    indexers' <- check_indexers indexers
    let type' = gen_type btype (length indexers')
    init' <- check_const_init_val type' init_
    newSymbol (SymInfo name type' Nothing)
    pure (VarDefInit name indexers' init')

gen_type :: BType -> Int -> TermType
gen_type btype indexers = type'
    where
        type' = if null dimensions then TermBType btype else TermArray btype dimensions
        dimensions = replicate indexers Nothing -- all nothing, to be computed and checked in next stage of static analysis

exp_type :: TypedExp -> TermType
exp_type (RawExp _) = Pre.error "Imposssible. Cannot call `exp_type` on non-checked expression"
exp_type (TypedExp t _) = t
exp_type (ConvExp t _) = t

check_block_unscoped :: Member SAEffects r => Block -> Sem r Block
check_block_unscoped (Block items) = Block <$> mapM check_block_item items
    where
        check_block_item (BlockItemDecl decl) = BlockItemDecl <$> check_decl decl
        check_block_item (BlockItemStmt stmt) = BlockItemStmt <$> check_stmt stmt

check_fparam :: Member SAEffects r => FuncFParam -> Sem r (TermType, FuncFParam)
check_fparam (FuncFParam btype name dimensions indexers) = do
    indexers' <- check_indexers indexers
    let type' = gen_type btype dimensions
    newSymbol (SymInfo name type' Nothing)
    pure (type', FuncFParam btype name dimensions indexers')

check_func :: Member SAEffects r => FuncDef -> Sem r FuncDef
check_func (FuncDef func_type name args block) = do
    withScope $ do -- function scope
        args' <- mapM check_fparam args
        let ret_type = funcType2TermType func_type
        let arg_types = fst <$> args'
        newFunc (FuncInfo name ret_type arg_types) -- insert function
        block' <- check_block_unscoped block -- block scope
        pure (FuncDef func_type name (snd <$> args') block')

check_top_level :: Member SAEffects r => TopLevel -> Sem r TopLevel
check_top_level (TLDecl decl) = TLDecl <$> check_decl decl
check_top_level (TLFun func) = TLFun <$> check_func func

check_comp_unit :: Member SAEffects r => CompUnit -> Sem r CompUnit
check_comp_unit (CompUnit tops) = CompUnit <$> mapM check_top_level tops

check_stmt :: Member SAEffects r => Stmt -> Sem r Stmt
check_stmt (StmtLVal lval e) = (StmtLVal . snd <$> check_lval lval) <*> check_typed_exp e
check_stmt (StmtExp Nothing) = pure (StmtExp Nothing)
check_stmt (StmtExp (Just e)) = StmtExp . Just <$> check_typed_exp e
check_stmt (StmtBlock block) = StmtBlock <$> withScope (check_block_unscoped block)
check_stmt (StmtIf cond then_ Nothing) = StmtIf <$> check_typed_exp cond <*> check_stmt then_ <*> pure Nothing
check_stmt (StmtIf cond then_ (Just else_)) = StmtIf <$> check_typed_exp cond <*> check_stmt then_ <*> (Just <$> check_stmt else_)
check_stmt (StmtWhile cond do_) = StmtWhile <$> check_typed_exp cond <*> check_stmt do_
check_stmt StmtBreak = pure StmtBreak
check_stmt StmtContinue = pure StmtContinue
check_stmt (StmtReturn Nothing) = pure (StmtReturn Nothing)
check_stmt (StmtReturn (Just e)) = StmtReturn . Just <$> check_typed_exp e

check_const_init_val :: Member SAEffects r => TermType -> ConstInitVal -> Sem r ConstInitVal
check_const_init_val type_ (ci, InitValExp e) = do
    e' <- check_typed_exp e -- exp is of array type, which is not permitted
    e'' <- case exp_type e' of
        TermAny -> pure e'
        TermBType _ -> typeCheck_im type_ e -- check type implicitly, TODO: float to int in initval is not permitted in SysY2022
        TermVoid -> error "Initial value cannot be void" $> e'
        TermArray _ _ -> error "Initial value of array type must be literal" $> e'
    pure (ci, InitValExp e'')
check_const_init_val type_ (ci, InitValArray arr) = do
    case type_ of
        TermArray _ _ -> pure () -- compile-time constants are computed in next stage
        _ -> error "Array-like initial value assigned to non-array symbol"
    pure (ci, InitValArray arr)
