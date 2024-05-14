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
import Control.Monad (forM, forM_, unless, join)
import Prelude hiding (error)
import Data.Maybe (catMaybes, mapMaybe)

sa_error :: Member SAEffects r => String -> Sem r ()
sa_error = SysY.StaticAnalysis.Basic.error

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
sa_var_def btype (VarDefUninit n indexers) = do
    indexers_ <- sa_indexers indexers
    type_ <- sa_gen_type btype indexers_
    newSymbol $ SymInfo n type_ False
    pure $ VarDefUninit n indexers_

sa_var_def btype (VarDefInit n indexers init_val) = do
    indexers_ <- sa_indexers indexers
    type_ <- sa_gen_type btype indexers_
    init_val_ <- sa_initval init_val
    sa_typecheck_initval type_ init_val_ -- check type
    newSymbol $ SymInfo n type_ False
    pure $ VarDefInit n indexers_ init_val_

sa_const_decl :: Member SAEffects r => ConstDecl -> Sem r ConstDecl
sa_const_decl (ConstDecl btype defs) = do
    ConstDecl btype <$> forM defs (sa_const_def btype)

sa_const_def :: Member SAEffects r => BType -> ConstDef -> Sem r ConstDef
sa_const_def btype (ConstDef n indexers init_val) = do
    indexers_ <- sa_indexers indexers
    type_ <- sa_gen_type btype indexers_
    init_val_ <- sa_initval init_val
    unless (hasConst init_val_) $ error "Expected compile-time constant"
    sa_typecheck_initval type_ init_val_ -- check type
    newSymbol $ SymInfo n type_ True
    pure $ ConstDef n indexers_ init_val_

sa_initval :: Member SAEffects r => ConstInitVal -> Sem r ConstInitVal
sa_initval (_, InitValExp e) = do
    e' <- sa_exp e
    c <- sa_compute_const e
    pure $ (ConstInfo c, InitValExp e')
sa_initval (_, InitValArray is) = do
    is' <- mapM sa_initval is
    let is_const = all hasConst is' -- TODO: locate errors
    let arr = mapMaybe constVal is'
    pure $ (ConstInfo (if is_const then Just (ConstValArray arr) else Nothing), InitValArray is')

sa_indexers :: Member SAEffects r => [TypedExp] -> Sem r [TypedExp]
sa_indexers = mapM (sa_exp_type (TermBType BInt))

sa_dimensions_const :: Member SAEffects r => [TypedExp] -> Sem r [Maybe Integer]
sa_dimensions_const indexers = forM indexers $ \x -> do
    c <- sa_compute_const_int x -- check for Integer constant
    case c of
        Nothing -> do
            sa_error "Dimensions of array declaration must be non-negative integer constant"
            pure Nothing
        Just t -> do
            if t < 0
                then do
                    sa_error "Dimensions of array declaration must be non-negative integer constant"
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


sa_compute_const :: Member SAEffects r => TypedExp -> Sem r (Maybe ConstVal)
sa_compute_const (_, ExpLVal (LVal name indexers)) = do
    indexers_ <- sa_indexers indexers
    withSymbol name Nothing $ \(SymInfo _ _ is_const) -> do
        if not is_const
            then pure Nothing
            else undefined
sa_compute_const _ = undefined

sa_compute_const_int :: Member SAEffects r => TypedExp -> Sem r (Maybe Integer)
sa_compute_const_int e = do
    c <- sa_compute_const e
    pure $ (go =<< c)
    where
        go (ConstValInt i) = Just i
        go _ = Nothing

-- class SAConst a where
--     tryGetConst :: Member SAEffects r => TypedExp -> Sem r (Maybe a)
-- instance SAConst Integer where
--     tryGetConst (_, ExpLVal (LVal name indexers)) = do
--         indexers_ <- sa_indexers indexers
--         withSymbol name Nothing $ \(SymInfo _ _ is_const) -> do
--             if not is_const
--                 then pure Nothing
--                 else undefined
--     tryGetConst _ = undefined

-- instance SAConst Float where
--     tryGetConst = undefined

sa_stmt :: Member SAEffects r => Stmt -> Sem r Stmt
sa_stmt _ = undefined

