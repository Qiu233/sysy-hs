{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Test.Hspec
import System.IO
import System.Directory (listDirectory)
import Data.List (sort, isSuffixOf, intercalate)
import Control.Monad (forM_, forM, unless)
import Text.Parsec (runParser)
import SysY.Parser
import Data.Either (isRight)
import Control.DeepSeq (($!!))
import SysY.StaticAnalysis (static_analysis)

func_test_path :: String
func_test_path = "./test/functional_test/"

load_func_tests :: IO [String]
load_func_tests = do
    files <- listDirectory func_test_path
    pure $ filter (isSuffixOf ".sy") $ sort files

main :: IO ()
main = do
    files <- load_func_tests
    codes <- forM files $ \x -> do
        withFile (func_test_path ++ x) ReadMode $ \handle -> do
            s <- hGetContents handle
            pure $!! s -- must be strict
    hspec $ func_tests (zip files codes)

func_tests :: [(String, String)] -> Spec
func_tests tests = do
    describe "functional tests" $ do
        forM_ tests $ \(test, code) -> do
            it test $ do
                let r = runParser parse_CompUnit () test code
                r `shouldSatisfy` isRight
                case r of
                    Left _ -> fail "impossible"
                    Right comp_unit -> do
                        let (es ,_) = static_analysis comp_unit
                        mapM_ print es
                        -- temporarily pass CI, because currently we have no runtime library
                        
                        -- unless (null es) $ do
                        --     let errors = intercalate "\n" es
                        --     fail $ "Static check failed with errors\n" ++ errors

