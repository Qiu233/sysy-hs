{-# LANGUAGE 
    TemplateHaskell, LambdaCase, BlockArguments, GADTs, FlexibleContexts, 
    TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
module SysY.Compiler.Basic where

import Polysemy



data CompilerMessage m a where
    Warn :: String -> CompilerMessage m ()
    Error :: String -> CompilerMessage m ()

makeSem ''CompilerMessage

