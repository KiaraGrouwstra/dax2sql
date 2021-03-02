{-# LANGUAGE OverloadedStrings #-}
-- | An example module.
module Example (main) where
import qualified SpreadsheetMLFormula as Formula
import           Data.Attoparsec.Text

-- TODO: [DAX](https://docs.microsoft.com/en-us/dax/dax-syntax-reference)
-- TODO: [MDX](https://github.com/drywolf/mdx-grammar/blob/master/grammars/oracle/mdx_grammar.txt)
-- TODO: [M](https://docs.microsoft.com/en-us/powerquery-m/m-spec-consolidated-grammar)

-- | An example function.
main :: IO ()
main = do
    -- putStrLn "hi"

    -- let query = "=1"
    let query = "=SUM(Customers)"
    -- let query = "=SUM(Customers[ID])"
    let parsed = parseOnly Formula.formula query
    case parsed of
        Left str -> putStrLn str
        Right res -> putStrLn $ show res
