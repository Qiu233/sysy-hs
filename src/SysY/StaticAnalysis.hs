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
import Control.Monad (forM, forM_)

-- sa_error :: Member SAEffects r => String -> Sem r ()
-- sa_error = SysY.StaticAnalysis.Basic.error

-- sa_exp :: Member SAEffects r => Exp -> Sem r TypedExp
-- sa_exp = interpretTypeCheckBySAEffects . type_infer

-- sa_stmt :: Member SAEffects r => Stmt -> Sem r TypedStmt
-- sa_stmt _ = undefined

-- bType2SysYBType :: BType -> SysYBType
-- bType2SysYBType BInt = SyInt
-- bType2SysYBType BFloat = SyFloat

-- bType2Typed :: BType -> TypedBType
-- bType2Typed BInt = TypedBInt
-- bType2Typed BFloat = TypedBFloat

-- sa_block_item :: Member SAEffects r => BlockItem -> Sem r TypedBlockItem
-- sa_block_item bi = do
--     case bi of
--         BlockItemStmt stmt -> TypedBlockItemStmt <$> sa_stmt stmt
--         BlockItemDecl (DeclConst d) -> TypedBlockItemDecl . TypedDeclConst <$> sa_const_decl d
--         BlockItemDecl (DeclVar d) -> TypedBlockItemDecl . TypedDeclVar <$> sa_var_decl d

-- sa_var_decl :: Member SAEffects r => VarDecl -> Sem r TypedVarDecl
-- sa_var_decl (VarDecl btype defs) = do
--     let btype' = bType2Typed btype
--     TypedVarDecl btype' <$> forM defs (sa_var_def btype)

-- sa_var_def :: Member SAEffects r => BType -> VarDef -> Sem r TypedVarDef
-- sa_var_def btype (VarDefUninit n indexers) = do
--     indexers_ <- sa_indexers indexers
--     type_ <- sa_gen_type btype indexers_
--     newSymbol $ SymInfo n type_
--     pure $ TypedVarDefUninit n indexers_

-- sa_var_def btype (VarDefInit n indexers init_val) = do
--     indexers_ <- sa_indexers indexers
--     type_ <- sa_gen_type btype indexers_
--     init_val_ <- sa_initval init_val
--     newSymbol $ SymInfo n type_
--     pure $ TypedVarDefInit n indexers_ init_val_

-- sa_const_decl :: Member SAEffects r => ConstDecl -> Sem r TypedConstDecl
-- sa_const_decl (ConstDecl btype defs) = do
--     let btype' = bType2Typed btype
--     TypedConstDecl btype' <$> forM defs (sa_const_def btype)

-- sa_const_def :: Member SAEffects r => BType -> ConstDef -> Sem r TypedConstDef
-- sa_const_def btype (ConstDef n indexers init_val) = do
--     indexers_ <- sa_indexers indexers
--     type_ <- sa_gen_type btype indexers_
--     init_val_ <- sa_const_initval init_val
--     newSymbol $ SymInfo n type_
--     pure $ TypedConstDef n indexers_ init_val_

-- sa_initval :: Member SAEffects r => InitVal -> Sem r TypedInitVal
-- sa_initval init_val = do
--     undefined

-- sa_const_initval :: Member SAEffects r => ConstInitVal -> Sem r TypedConstInitVal
-- sa_const_initval init_val = do
--     undefined

-- -- sa_new_decl :: Member SAEffects r => Ident -> BType -> [Exp] -> Sem r TermType
-- -- sa_new_decl n btype indexers = do
-- --     type_ <- sa_gen_type btype indexers
-- --     newSymbol $ SymInfo n type_
-- --     pure type_


-- sa_indexers :: Member SAEffects r => [Exp] -> Sem r [TypedExp]
-- sa_indexers = mapM sa_exp

-- sa_indexers_const :: Member SAEffects r => [TypedExp] -> Sem r [Integer]
-- sa_indexers_const indexers = forM indexers $ \x -> do
--     c <- tryGetConst @Integer x -- check for Integer constant
--     case c of
--         Nothing -> do
--             sa_error "Dimensions of array declaration must be constant"
--             pure $ -1 -- TODO: don't use special value
--         Just t -> pure t

-- sa_gen_type :: Member SAEffects r => BType -> [TypedExp] -> Sem r TermType
-- sa_gen_type btype indexers = do
--     indexers_ <- sa_indexers_const indexers
--     pure $ if null indexers
--         then TermBType $ bType2SysYBType btype
--         else TermArray (bType2SysYBType btype) indexers_


-- sa_typecheck_initval :: Member SAEffects r => TermType -> TypedInitVal -> Sem r ()
-- sa_typecheck_initval t v = interpretTypeCheckBySAEffects $ typeCheckInitVal t v
-- sa_typecheck_const_initval :: Member SAEffects r => TermType -> TypedConstInitVal -> Sem r ()
-- sa_typecheck_const_initval t v = interpretTypeCheckBySAEffects $ typeCheckConstInitVal t v

-- -- sa_infer :: (Member SAEffects r, TypeInfer a b) => a -> Sem r b
-- -- sa_infer = interpretTypeCheckBySAEffects . type_infer

-- class SAConst a where
--     tryGetConst :: Member SAEffects r => TypedExp -> Sem r (Maybe a)

-- instance SAConst Integer where
--     tryGetConst = undefined
-- instance SAConst Float where
--     tryGetConst = undefined


