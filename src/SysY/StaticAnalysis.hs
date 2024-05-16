{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module SysY.StaticAnalysis (
    module SysY.StaticAnalysis.Basic,
    static_analysis_pure
) where

import SysY.StaticAnalysis.Basic
import SysY.StaticAnalysis.NameRes as NR
import SysY.StaticAnalysis.TypeCheck as TC
import Prelude hiding (error)
import SysY.AST.Basic
import Control.Monad.State

import Control.Monad.Except
import Control.Monad (when)
import SysY.StaticAnalysis.Pure
import Polysemy

sa_lift :: Member SAEffects r => (CompUnit -> Sem r CompUnit) -> ExceptT () (StateT CompUnit (Sem r)) ()
sa_lift p = do
    c <- get
    c' <- lift $ lift $ p c
    put c'
    e <- lift $ lift hasError
    when e $ throwError () -- exit static analysis

static_analysis_effectful_inner :: Member SAEffects r => ExceptT () (StateT CompUnit (Sem r)) ()
static_analysis_effectful_inner = do
    sa_lift NR.check_comp_unit
    sa_lift TC.check_comp_unit

static_analysis_effectful :: Member SAEffects r => CompUnit -> Sem r CompUnit
static_analysis_effectful c = do
    let ex = runExceptT static_analysis_effectful_inner
    execStateT ex c

static_analysis_pure :: CompUnit -> ([String], [String], CompUnit)

static_analysis_pure c = do
    let s = runSAEffectsPure (static_analysis_effectful c)
    let (c', SAContext ws es _ _) = runState s defaultSAContext
    (ws, es, c)
