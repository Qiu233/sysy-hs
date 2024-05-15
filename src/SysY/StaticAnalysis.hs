{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module SysY.StaticAnalysis (
    module SysY.StaticAnalysis.Basic,
    module SysY.StaticAnalysis.TypeCheck,
    static_analysis
) where

import SysY.StaticAnalysis.NameRes
import SysY.StaticAnalysis.Basic
import SysY.StaticAnalysis.TypeCheck
import Prelude hiding (error)
import SysY.AST.Basic
import Control.Monad.State

import Control.Monad.Except
import Control.Monad (when)
import Control.Lens


checkNameRes :: State (CompUnit, SAContext) ()
checkNameRes = do
    comp_unit <- gets fst
    zoom _2 $ do
        runSAEffectsPure $ SysY.StaticAnalysis.NameRes.check_comp_unit comp_unit

checkType :: State (CompUnit, SAContext) ()
checkType = do
    comp_unit <- gets fst
    comp_unit' <- zoom _2 $ do
        runSAEffectsPure $ SysY.StaticAnalysis.TypeCheck.check_comp_unit comp_unit
    _1 .= comp_unit'

terminate_if_has_error :: ExceptT () (State (CompUnit, SAContext)) ()
terminate_if_has_error = do
    e <- lift (zoom _2 has_error)
    when e $ throwError ()
    lift $ zoom _2 reset_symbols

static_analysis_inner :: ExceptT () (State (CompUnit, SAContext)) ()
static_analysis_inner = do
    lift $ checkNameRes
    terminate_if_has_error
    lift $ checkType
    terminate_if_has_error

static_analysis :: CompUnit -> ([String], CompUnit) -- TODO: rewrite for effect stack
static_analysis c = do
    let ex = runExceptT static_analysis_inner
    let (c', SAContext ws es _ _) = execState ex (c, defaultSAContext)
    (es, c)

