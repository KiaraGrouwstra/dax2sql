{-# LANGUAGE OverloadedStrings #-}
import qualified Test.Tasty
import           Test.Tasty.Hspec
import qualified SpreadsheetMLFormula as Formula
import           Data.Attoparsec.Text

-- https://hackage.haskell.org/package/sqlite-simple

main :: IO ()
main = do
    test <- testSpec "dax2sql" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "is trivially true" $ do
        -- let query = "=1"
        let query = "=SUM(Customers)"
        -- let query = "=SUM(Customers[ID])"
        let parsed = parseOnly Formula.formula query
        case parsed of
            Left str -> putStrLn str
            Right res -> putStrLn $ show res
        True `shouldBe` True
