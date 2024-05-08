module Main (main) where

import Lib
import Text.Parsec

test1 = "int a = 0 + 2;\nvoid main(){return 0;}"

test2 = "float a = 0x0.2p2;"

test_parse = runParser parse_CompUnit () ""

main :: IO ()
main = do
    print $ test_parse test1
    print $ test_parse test2

