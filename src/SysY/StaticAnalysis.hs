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



checkNameRes :: CompUnit -> State SAContext ()
checkNameRes comp_unit = runSAEffectsPure $ SysY.StaticAnalysis.NameRes.check_comp_unit comp_unit


static_analysis :: CompUnit -> ([String], CompUnit) -- TODO: rewrite for effect stack
static_analysis c = do
    let SAContext ws es _ _ = execState (checkNameRes c) defaultSAContext
    -- mapM_ print es
    (es, c)

