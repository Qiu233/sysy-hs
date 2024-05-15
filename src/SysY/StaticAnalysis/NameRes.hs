{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module SysY.StaticAnalysis.NameRes (
    check_comp_unit
) where
import Polysemy
import SysY.AST
import SysY.StaticAnalysis.Basic
import Prelude hiding (error)
import qualified Prelude
import Text.Printf (printf)
import Control.Monad (unless)
import Data.Maybe (isJust)

new_symbol :: Member SAEffects r => Ident -> Sem r ()
new_symbol name = do
    findSymbol name >>= \case
        Just (_, True) -> error $ printf "Duplicate declarations of variable '%s'" name
        _ -> pure ()
    newSymbol (SymInfo name TermAny Nothing) -- override the last one

new_function :: Member SAEffects r => Ident -> Sem r ()
new_function name = do
    findFunc name >>= \case
        Just _ -> error $ printf "Duplicate declarations of function '%s'" name
        _ -> newFunc (FuncInfo name TermAny [])
    -- don't override last function definition

exists_symbol :: Member SAEffects r => Ident -> Sem r Bool
exists_symbol name = isJust <$> findSymbol name

assert_symbol :: Member SAEffects r => Ident -> Sem r ()
assert_symbol name = do
    e <- exists_symbol name
    unless e $ do
        error $ printf "Variable %s is used but not defined" name

exists_function :: Member SAEffects r => Ident -> Sem r Bool
exists_function name = isJust <$> findFunc name

assert_function :: Member SAEffects r => Ident -> Sem r ()
assert_function name = do
    e <- exists_function name
    unless e $ do
        error $ printf "Function %s is used but not defined" name

check_lval :: Member SAEffects r => LVal -> Sem r ()
check_lval (LVal name _) = assert_symbol name

check_typed_exp :: Member SAEffects r => TypedExp -> Sem r ()
-- check_typed_exp = check_exp. snd
check_typed_exp (RawExp e) = check_exp e
check_typed_exp _ = Prelude.error "Impossible. Type checking cannot preced name resolution"

check_exp :: Member SAEffects r => Exp -> Sem r ()
-- check_exp = \case -- TODO: have different variant definitions
check_exp (ExpLVal l) = check_lval l
check_exp (ExpNum _) = pure ()
check_exp (ExpOpUnary _ oprd) = check_typed_exp oprd
check_exp (ExpOpBinary _ lhs rhs) = check_typed_exp lhs >> check_typed_exp rhs
check_exp (ExpCall name args) = do
    assert_function name
    mapM_ check_typed_exp args

check_block_unscoped :: Member SAEffects r => Block -> Sem r ()
check_block_unscoped (Block items) = mapM_ check_block_item items
    where
        check_block_item (BlockItemDecl decl) = check_decl decl
        check_block_item (BlockItemStmt stmt) = check_stmt stmt

check_stmt :: Member SAEffects r => Stmt -> Sem r ()
check_stmt (StmtLVal lval e) = check_lval lval >> check_typed_exp e
check_stmt (StmtExp Nothing) = pure ()
check_stmt (StmtExp (Just e)) = check_typed_exp e
check_stmt (StmtBlock block) = withScope $ check_block_unscoped block
check_stmt (StmtIf cond then' Nothing) = check_typed_exp cond >> check_stmt then'
check_stmt (StmtIf cond then' (Just else')) = check_typed_exp cond >> check_stmt then' >> check_stmt else'
check_stmt (StmtWhile cond do') = check_typed_exp cond >> check_stmt do'
check_stmt StmtBreak = pure ()
check_stmt StmtContinue = pure ()
check_stmt (StmtReturn Nothing) = pure ()
check_stmt (StmtReturn (Just e)) = check_typed_exp e

check_decl :: Member SAEffects r => Decl -> Sem r ()
check_decl (DeclConst (ConstDecl _ defs)) = mapM_ check_const_def defs
check_decl (DeclVar (VarDecl _ defs)) = mapM_ check_var_def defs

check_const_def :: Member SAEffects r => ConstDef -> Sem r ()
check_const_def (ConstDef name indexers init_) = do
    mapM_ check_typed_exp indexers
    check_const_init_val init_
    new_symbol name -- must after all checks

check_const_init_val :: Member SAEffects r => ConstInitVal -> Sem r ()
check_const_init_val = check_init_val . snd

check_init_val :: Member SAEffects r => InitVal -> Sem r ()
check_init_val (InitValExp e) = check_typed_exp e
check_init_val (InitValArray comps) = mapM_ check_const_init_val comps

check_var_def :: Member SAEffects r => VarDef -> Sem r ()
check_var_def (VarDefUninit name indexers) = do
    mapM_ check_typed_exp indexers
    new_symbol name
check_var_def (VarDefInit name indexers init_) = do
    mapM_ check_typed_exp indexers
    check_const_init_val init_
    new_symbol name

check_fparam :: Member SAEffects r => FuncFParam -> Sem r ()
check_fparam (FuncFParam _ name _ indexers) = do
    mapM_ check_typed_exp indexers
    new_symbol name

check_func :: Member SAEffects r => FuncDef -> Sem r ()
check_func (FuncDef _ name args block) = do
    new_function name -- insert function symbol so it can be recursive
    withScope $ do -- function scope
        mapM_ check_fparam args
        check_block_unscoped block -- block scope

check_top_level :: Member SAEffects r => TopLevel -> Sem r ()
check_top_level (TLDecl decl) = check_decl decl
check_top_level (TLFun func) = check_func func

check_comp_unit :: Member SAEffects r => CompUnit -> Sem r ()
check_comp_unit (CompUnit tops) = mapM_ check_top_level tops



