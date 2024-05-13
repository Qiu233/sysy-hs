module Main (main) where

import SysY
import Text.Parsec
import System.Directory

test1 = "int a = 0 + 2;\nvoid main(){return 0;}"

test2 = "float a = 0x0.2p2;"

test_parse = runParser parse_CompUnit () ""

main :: IO ()
main = do
    a <- getCurrentDirectory
    print a
    print $ test_parse test1
    print $ test_parse test2

