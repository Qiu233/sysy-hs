{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.Hspec
import System.IO
import System.Directory (listDirectory)
import Data.Functor (void)
import Data.List (sort, isSuffixOf)
import Control.Monad (forM_)
import Text.Parsec (runParser)
import SysY.Parser

func_test_path :: String
func_test_path = "./test/functional_test/"

load_func_tests :: IO [String]
load_func_tests = do
    files <- listDirectory func_test_path
    pure $ filter (isSuffixOf ".sy") $ sort files

main :: IO ()
main = do
    files <- load_func_tests
    forM_ files $ \x -> do
        handle <- openFile (func_test_path ++ x) ReadMode
        content <- hGetContents handle
        let r = runParser parse_CompUnit () x content
        case r of
            Right _ -> putStrLn $ "test '" ++ x ++ "' passed"
            Left err -> putStrLn $ "test '" ++ x ++ "' failed with error: " ++ show err
        hClose handle
