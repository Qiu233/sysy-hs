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
import Control.Monad (forM, forM_, unless, join, zipWithM)
import Prelude hiding (error)
import Data.Maybe (catMaybes, mapMaybe, isJust, fromJust)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (lift)
import Data.Int
import Data.Bits (complement)
import GHC.Num (integerDiv)
