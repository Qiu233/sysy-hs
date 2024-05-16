module Main (main) where

import SysY
import Text.Parsec
import System.Directory
import SysY.StaticAnalysis
import Control.Monad (void, mapM_)

test1 = "int a = 0 + 2;\nvoid main(){return a;}"

-- test2 = "float a = 0x0.2p2;"

test_parse = runParser parse_CompUnit () ""

main :: IO ()
main = do
    let parsed = test_parse test1
    print parsed
    case parsed of
        Left _ -> pure ()
        Right comp_unit -> do
            let (_, es, _) = static_analysis_pure comp_unit
            mapM_ print es
            pure ()

