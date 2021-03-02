{-# LANGUAGE OverloadedStrings #-}
-- | [ISO/IEC 29500-1:2016](https://www.iso.org/standard/71691.html) 18.17 SpreadsheetML Formulas
module SpreadsheetMLFormula (module SpreadsheetMLFormula) where

import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Data.Functor    (void)
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Attoparsec.Expr
import           Data.Scientific (Scientific)

-- characters

fullStop :: Parser Char
fullStop = char '.' <?> "fullStop"

doubleQuote :: Parser Char
doubleQuote = char '"' <?> "doubleQuote"

comma :: Parser Char
comma = char ',' <?> "comma"

colon :: Parser Char
colon = char ':' <?> "colon"

apostrophe :: Parser Char
apostrophe = char '\'' <?> "apostrophe"

bang :: Parser Char
bang = char '!' <?> "bang"

dollar :: Parser Char
dollar = char '$' <?> "dollar"

underscore :: Parser Char
underscore = char '_' <?> "underscore"

backslash :: Parser Char
backslash = char '\\' <?> "backslash"

-- expressions

formula :: Parser Expression
formula = char '=' *> expression <* endOfInput <?> "formula"

data Expression
    = ParenExpr Expression
    | ConstantExpr Constant
    -- -- | PrefixedExpr Prefix Expression
    -- | InfixExpr Expression Infix Expression
    -- | PostfixExpr Expression Postfix
    | RangeExpr Range
    | FunctionCall Function
    deriving (Show, Eq)

data Range
    = CellRef CellReference
    | NameExpr Name
    | RangeUnion Range Range
    | RangeIntersection Range Range
    | RefRange Range Range
    deriving (Show, Eq)

-- expression :: Parser Expression
-- expression = skipSpace *> choice
--     [ fmap ParenExpr parenExpr
--     , fmap ConstantExpr constant
--     -- , prefixedExpr
--     , infixExpr
--     , postfixExpr
--     , CellRef <$> cellReference
--     , FunctionCall <$> functionCall
--     , fmap NameExpr name
--     ] <* skipSpace <?> "expression"

expression :: Parser Expression
expression = buildExpressionParser table term
        <?> "expression"

term :: Parser Expression
term = choice
    [ fmap ParenExpr parenExpr
    , fmap ConstantExpr constant
    , CellRef <$> cellReference
    , FunctionCall <$> functionCall
    , fmap NameExpr name
    ] <?> "simple expression"

binary  name fun assoc = Infix (do{ string name; return fun }) assoc
prefix  name fun       = Prefix (do{ string name; return fun })
postfix name fun       = Postfix (do{ string name; return fun })

-- TODO: figure out sensible precedence, associativity
infixl 5 <->
(<->) = concatParse
concatParse :: (Monad m, Semigroup a) => m a -> m a -> m a
concatParse l r = do
    l' <- l
    r' <- r
    pure $ l' <> r'

parenExpr :: Parser Expression
parenExpr = char '(' *> skipSpace *> expression <* skipSpace <* char ')' <?> "parenExpr"

-- prefixedExpr :: Parser Expression
-- prefixedExpr = do
--     op <- prefixOperator
--     skipSpace
--     r <- expression
--     let expr = PrefixedExpr op r 
--     pure expr <?> "prefixedExpr"

-- infixExpr :: Parser Expression
-- infixExpr = do
--     l <- expression
--     skipSpace
--     op <- infixOperator
--     skipSpace
--     r <- expression
--     let expr = InfixExpr l op r
--     pure expr <?> "infixExpr"

-- postfixExpr :: Parser Expression
-- postfixExpr = do
--     l <- expression
--     skipSpace
--     op <- postfixOperator
--     let expr = PostfixExpr l op
--     pure expr <?> "postfixExpr"

data Constant
    = Error ErrorType
    | Logical Bool
    | Numerical Scientific
    | StringConstant T.Text
    | ArrayConstant [[Constant]]
    deriving (Show, Eq)

constant :: Parser Constant
constant = choice
    [ (Error <$> errorConstant)
    , (Logical <$> logicalConstant)
    , (Numerical <$> numericalConstant)
    , (StringConstant <$> stringConstant)
    -- , (ArrayConstant <$> arrayConstant)
    ] <?> "constant"

data ErrorType
    = DIV0Error
    | NAError
    | NameError
    | NullError
    | NumError
    | RefError
    | ValueError
    | GettingDataError
    deriving (Show, Eq)

-- type: error
errorConstant :: Parser ErrorType
errorConstant = choice
    [ DIV0Error <$ asciiCI "#DIV/0!"
    , NAError <$ asciiCI "#N/A"
    , NameError <$ asciiCI "#NAME?"
    , NullError <$ asciiCI "#NULL!"
    , NumError <$ asciiCI "#NUM!"
    , RefError <$ asciiCI "#REF!"
    , ValueError <$ asciiCI "#VALUE!"
    , GettingDataError <$ asciiCI "#GETTING_DATA"
    ] <?> "errorConstant"

-- type: logical
logicalConstant :: Parser Bool
logicalConstant = choice
    [ False <$ asciiCI "FALSE"
    , True <$ asciiCI "TRUE"
    ] <?> "logicalConstant"

-- data NumericalValue
--     = IntVal Int
--     | FloatVal Float
--     deriving (Show, Eq)

-- type: number
numericalConstant :: Parser Scientific
numericalConstant = scientific <?> "numericalConstant"

-- wholeNumberPart :: Parser T.Text
-- wholeNumberPart = digitSequence <?> "wholeNumberPart"

-- fractionalPart :: Parser T.Text
-- fractionalPart = digitSequence <?> "fractionalPart"

-- exponentPart :: Parser (Sign, Int)
-- exponentPart = do
--     void $ asciiCI "e"
--     sign_ <- option Positive sign
--     digits <- digitSequence
--     pure (sign_, digits) <?> "exponentPart"

-- data Sign = Positive | Negative
--     deriving (Show, Eq)

-- sign :: Parser Sign
-- sign = choice
--     [ Positive <$ char '+'
--     , Negative <$ char '-'
--     ] <?> "sign"

digitSequence :: Parser Int
digitSequence = decimal <?> "digitSequence"

-- type: string
stringConstant :: Parser T.Text
stringConstant = doubleQuote *> stringChars <* doubleQuote <?> "stringConstant"

stringChars :: Parser T.Text
stringChars = T.pack <$> many' stringChar <?> "stringChars"

stringChar :: Parser Char
stringChar = choice
    [ '"' <$ string "\"\""
    , notChar '"'
    ] <?> "stringChar"

-- type: array
-- An array-constant shall not contain
-- - An array-constant.
-- - Columns or rows of unequal length
arrayConstant :: Parser [[Constant]]
arrayConstant = char '{' *> constantListRows <* char '}' <?> "arrayConstant"

constantListRows :: Parser [[Constant]]
constantListRows = constantListRow `sepBy` (char ';') <?> "constantListRows"

constantListRow :: Parser [Constant]
constantListRow = constant `sepBy` comma <?> "constantListRow"

-- operator :: Parser T.Text
-- operator = choice
--     [ infixOperator
--     , postfixOperator
--     , prefixOperator
--     ] <?> "operator"

-- data Postfix
--     = Percent
--     deriving (Show, Eq)

-- postfixOperator :: Parser Postfix
-- postfixOperator = Percent <$ char '%' <?> "postfixOperator"

-- -- data Prefix
-- --     = Plus
-- --     | Minus
-- --     deriving (Show, Eq)

-- -- prefixOperator :: Parser Prefix
-- -- prefixOperator =
-- --     [ Plus <$ char '+'
-- --     , Minus <$ char '-'
-- --     ] <?> "prefixOperator"

-- data Infix
--     = NotEqual -- 0
--     | GreaterEqual -- 0
--     | LessEqual -- 0
--     | And -- ?
--     | Or -- ?
--     | Comma -- 7
--     | Space -- 7
--     | Colon -- 7
--     | Caret -- 4
--     | Asterisk -- 3
--     | Slash -- 3
--     | PlusInfix -- 2
--     | MinusInfix -- 2
--     | Ampersand -- 1
--     | Equals -- 0
--     | LessThan -- 0
--     | GreaterThan -- 0
--     deriving (Show, Eq)

table = [ [ binary "," RangeUnion AssocLeft
          , binary space RangeIntersection AssocLeft
          , binary ":" RefRange AssocLeft
          ]
        , [ prefix "-" negate
          -- , prefix "+" id
          ]
        , [ postfix "%" (* 0.01) ]
        , [ binary "^" exponentiation AssocLeft ]
        , [ binary "*" (*) AssocLeft
          , binary "/" (div) AssocLeft
          ]
        , [ binary "+" (+) AssocLeft
          , binary "-" (-)   AssocLeft
          ]
        , [ binary "&" (<>) AssocLeft ]
        , [ binary "=" (==) AssocLeft
          , binary "<>" (/=) AssocLeft
          , binary ">=" (>=) AssocLeft
          , binary "<=" (<=) AssocLeft
          , binary ">" (>) AssocLeft
          , binary "<" (<) AssocLeft
          ]
        -- , [ binary "&&" (&&) AssocLeft
        --   , binary "||" (||) AssocLeft
        --   ]
        ]
        
-- infixOperator :: Parser Infix
-- infixOperator = choice
--     [ NotEqual <$ string "<>"
--     , GreaterEqual <$ string ">="
--     , LessEqual <$ string "<="
--     , And <$ string "&&" -- DAX
--     , Or <$ string "||" -- DAX
--     , Comma <$ comma
--     , Space <$ space
--     , Colon <$ char ':'
--     , Caret <$ char '^'
--     , Asterisk <$ char '*'
--     , Slash <$ char '/'
--     , PlusInfix <$ char '+'
--     , MinusInfix <$ char '-'
--     , Ampersand <$ char '&'
--     , Equals <$ char '='
--     , LessThan <$ char '<'
--     , GreaterThan <$ char '>'
--     ] <?> "infixOperator"

-- cell references

data CellReference
    = NamedReference Name
    | PrefixedA1Ref WorksheetPrefix A1Reference (Maybe A1Reference)
    | PrefixedR1C1Ref WorksheetPrefix R1c1Reference (Maybe R1c1Reference)
    deriving (Show, Eq)

cellReference :: Parser CellReference
cellReference = choice
    [ NamedReference <$> name
    , do
        sheet <- worksheetPrefix
        fromRef <- a1Reference
        toRef <- maybeParse (colon *> a1Reference)
        pure $ PrefixedA1Ref sheet fromRef toRef
    , do
        sheet <- worksheetPrefix
        fromRef <- r1c1Reference
        toRef <- maybeParse (colon *> r1c1Reference)
        pure $ PrefixedR1C1Ref sheet fromRef toRef
    ] <?> "cellReference"

data WorksheetPrefix = WorksheetPrefix
    { wbName :: Maybe T.Text
    , fromSheet :: T.Text
    , toSheet :: Maybe T.Text
    }
    deriving (Show, Eq)

worksheetPrefix :: Parser WorksheetPrefix
worksheetPrefix = choice
    [ worksheetPrefixSpecial
    , worksheetPrefixUnquoted
    ] <?> "worksheetPrefix"

worksheetPrefixUnquoted :: Parser WorksheetPrefix
worksheetPrefixUnquoted = do
    wb <- maybeParse (char '[' *> workbookName <* char ']')
    fromSheet <- sheetName
    toSheet <- maybeParse (colon *> sheetName)
    void bang
    let res = WorksheetPrefix wb fromSheet toSheet
    pure res <?> "worksheetPrefixUnquoted"

worksheetPrefixSpecial :: Parser WorksheetPrefix
worksheetPrefixSpecial = do
    void apostrophe
    wb <- maybeParse (char '[' *> workbookNameSpecial <* char ']')
    fromSheet <- sheetNameSpecial
    toSheet <- maybeParse (colon *> sheetNameSpecial)
    void apostrophe
    void bang
    let res = WorksheetPrefix wb fromSheet toSheet
    pure res <?> "worksheetPrefix"

workbookName :: Parser T.Text
workbookName = bookNameCharacters <?> "workbookName"

bookNameCharacters :: Parser T.Text
bookNameCharacters = T.pack <$> many1 bookNameCharacter <?> "bookNameCharacters"

-- technically, any character except operator or ', [, ], or ?
bookNameCharacter :: Parser Char
bookNameCharacter = letter <?> "bookNameCharacter"

sheetName :: Parser T.Text
sheetName = sheetNameCharacters <?> "sheetName"

sheetNameCharacters :: Parser T.Text
sheetNameCharacters = T.pack <$> many1 sheetNameCharacter <?> "sheetNameCharacters"

-- technically, any character except operator or ', [, ], \, or ?
sheetNameCharacter :: Parser Char
sheetNameCharacter = letter <?> "sheetNameCharacter"

workbookNameSpecial :: Parser T.Text
workbookNameSpecial = bookNameStartCharacterSpecial <-> bookNameCharactersSpecial <?> "workbookNameSpecial"

bookNameCharactersSpecial :: Parser T.Text
bookNameCharactersSpecial = T.pack <$> many' bookNameCharacterSpecial <?> "bookNameCharactersSpecial"

-- technically, any character, including operator, except ', *, [, ], :, or ?
bookNameStartCharacterSpecial :: Parser T.Text
bookNameStartCharacterSpecial = T.pack . pure <$> letter <?> "bookNameStartCharacterSpecial"

bookNameCharacterSpecial :: Parser Char
bookNameCharacterSpecial = choice
    [ apostrophe *> apostrophe
    -- technically, any character, including operator, except ', *, [, ], :, or ?
    , letter
    ] <?> "bookNameCharacterSpecial"

sheetNameSpecial :: Parser T.Text
sheetNameSpecial = do
    l <- sheetNameStartCharacterSpecial
    r <- option "" (sheetNameCharactersSpecial <-> sheetNameEndCharacterSpecial)
    pure (l <> r) <?> "sheetNameSpecial"

sheetNameEndCharacterSpecial :: Parser T.Text
sheetNameEndCharacterSpecial = sheetNameStartCharacterSpecial <?> "sheetNameEndCharacterSpecial"

-- technically, any character, including operator, except ', *, [, ], \, :, /, or ?
sheetNameStartCharacterSpecial :: Parser T.Text
sheetNameStartCharacterSpecial = T.pack . pure <$> letter <?> "sheetNameStartCharacterSpecial"

sheetNameCharactersSpecial :: Parser T.Text
sheetNameCharactersSpecial = T.pack <$> many' sheetNameCharacterSpecial <?> "sheetNameCharactersSpecial"

sheetNameCharacterSpecial :: Parser Char
sheetNameCharacterSpecial = choice
    [ apostrophe *> apostrophe
    -- technically, any character, including operator, except ', *, [, ], \, :, or ?
    , letter
    ] <?> "sheetNameCharacterSpecial"

-- A1-style cell references

data A1Reference
    = A1Columns A1Column A1Column
    | A1Rows A1Row A1Row
    | A1ColRow A1Column A1Row
    deriving (Show, Eq)

a1Reference :: Parser A1Reference
a1Reference = choice
    [ do
        l <- a1Column
        void colon
        r <- a1Column
        pure (A1Columns l r)
    , do
        l <- a1Row
        void colon
        r <- a1Row
        pure (A1Rows l r)
    , do
        col <- a1Column
        void colon
        row <- a1Row
        pure (A1ColRow col row)
    ] <?> "a1Reference"

data A1Column
    = A1RelativeColumn T.Text
    | A1AbsoluteColumn T.Text
    deriving (Show, Eq)

a1Column :: Parser A1Column
a1Column = choice
    [ A1RelativeColumn <$> a1RelativeColumn
    , A1AbsoluteColumn <$> a1AbsoluteColumn
    ] <?> "a1Column"

a1RelativeColumn :: Parser T.Text
a1RelativeColumn = T.pack <$> many1 letter <?> "a1RelativeColumn"

a1AbsoluteColumn :: Parser T.Text
a1AbsoluteColumn = dollar *> a1RelativeColumn <?> "a1AbsoluteColumn"

data A1Row
    = A1RelativeRow
    | A1AbsoluteRow
    deriving (Show, Eq)

a1Row :: Parser A1Row
a1Row = choice
    [ A1RelativeRow <$ a1RelativeRow
    , A1AbsoluteRow <$ a1AbsoluteRow
    ] <?> "a1Row"

a1RelativeRow :: Parser Int
a1RelativeRow = digitSequence <?> "a1RelativeRow"

a1AbsoluteRow :: Parser Int
a1AbsoluteRow = dollar *> a1RelativeRow <?> "a1AbsoluteRow"

-- R1C1-style cell reference

data R1c1Reference
    = R1c1RowOnly R1c1Row
    | R1c1ColumnOnly R1c1Column
    | R1c1RowColumn R1c1Row R1c1Column
    deriving (Show, Eq)

r1c1Reference :: Parser R1c1Reference
r1c1Reference = choice
    [ R1c1RowOnly <$> r1c1RowOnly
    , R1c1ColumnOnly <$> r1c1ColumnOnly
    , r1c1RowColumn
    ] <?> "r1c1Reference"

r1c1RowColumn :: Parser R1c1Reference
r1c1RowColumn = do
    row <- r1c1Row
    col <- r1c1Column
    pure (R1c1RowColumn row col) <?> "r1c1RowColumn"

r1c1RowOnly :: Parser R1c1Row
r1c1RowOnly = choice
    [ R1c1AbsoluteRow . pure <$> (asciiCI "R" *> r1c1AbsoluteNumber)
    , R1c1RelativeRow <$> (asciiCI "R[" *> r1c1RelativeNumber <* char ']')
    ] <?> "r1c1RowOnly"

data R1c1Row
    = R1c1RelativeRow Int
    | R1c1AbsoluteRow (Maybe Int)
    deriving (Show, Eq)

r1c1Row :: Parser R1c1Row
r1c1Row = choice
    [ R1c1RelativeRow <$> r1c1RelativeRow
    , R1c1AbsoluteRow <$> r1c1AbsoluteRow
    ] <?> "r1c1Row"

r1c1RelativeRow :: Parser Int
r1c1RelativeRow = asciiCI "R[" *> r1c1RelativeNumber <* char ']' <?> "r1c1RelativeRow"

r1c1AbsoluteRow :: Parser (Maybe Int)
r1c1AbsoluteRow = asciiCI "R" *> maybeParse r1c1AbsoluteNumber <?> "r1c1AbsoluteRow"

r1c1ColumnOnly :: Parser R1c1Column
r1c1ColumnOnly = choice
    [ R1c1AbsoluteColumn . pure <$> (asciiCI "C" *> r1c1AbsoluteNumber)
    , R1c1RelativeColumn <$> (asciiCI "C[" *> r1c1RelativeNumber <* char ']')
    ] <?> "r1c1ColumnOnly"

data R1c1Column
    = R1c1RelativeColumn Int
    | R1c1AbsoluteColumn (Maybe Int)
    deriving (Show, Eq)

r1c1Column :: Parser R1c1Column
r1c1Column = choice
    [ R1c1RelativeColumn <$> r1c1RelativeColumn
    , R1c1AbsoluteColumn <$> r1c1AbsoluteColumn
    ] <?> "r1c1Column"

r1c1RelativeColumn :: Parser Int
r1c1RelativeColumn = asciiCI "C[" *> r1c1RelativeNumber <* char ']' <?> "r1c1RelativeColumn"

r1c1AbsoluteColumn :: Parser (Maybe Int)
r1c1AbsoluteColumn = asciiCI "C" *> maybeParse r1c1AbsoluteNumber <?> "r1c1AbsoluteColumn"

r1c1RelativeNumber :: Parser Int
r1c1RelativeNumber = signed digitSequence <?> "r1c1RelativeNumber"

r1c1AbsoluteNumber :: Parser Int
r1c1AbsoluteNumber = digitSequence <?> "r1c1AbsoluteNumber"

-- functions

data Function = Function
    { function :: T.Text
    , arguments :: [Expression]
    }
    deriving (Show, Eq)

functionCall :: Parser Function
functionCall = do
    fn <- functionName
    args <- char '(' *> skipSpace *> argumentList <* skipSpace <* char ')'
    let function = Function fn args
    pure function <?> "functionCall"

functionName :: Parser T.Text
functionName = choice
    [ prefixedFunctionName
    , predefinedFunctionName
    , userDefinedFunctionName
    ] <?> "functionName"

-- insert function names here
predefinedFunctionName :: Parser T.Text
predefinedFunctionName = choice
    [
    ] <?> "predefinedFunctionName"

prefixedFunctionName :: Parser T.Text
prefixedFunctionName = choice
    [ asciiCI "ISO." *> predefinedFunctionName
    , asciiCI "ECMA." *> predefinedFunctionName
    ] <?> "prefixedFunctionName"

userDefinedFunctionName :: Parser T.Text
userDefinedFunctionName = (T.pack . pure <$> letter) <-> userDefinedNameCharacters <?> "userDefinedFunctionName"

userDefinedNameCharacters :: Parser T.Text
userDefinedNameCharacters = T.pack <$> many' userDefinedNameCharacter <?> "userDefinedNameCharacters"

userDefinedNameCharacter :: Parser Char
userDefinedNameCharacter = choice
    [ letter
    , digit
    , fullStop
    ] <?> "userDefinedNameCharacter"

argumentList :: Parser [Expression]
argumentList = argument `sepBy` comma <?> "argumentList"

argument :: Parser Expression
argument = expression <?> "argument"

-- names

data Name = Name
    { workbook :: Maybe T.Text
    , refName :: T.Text
    } deriving (Show, Eq)

maybeParse :: Parser a -> Parser (Maybe a)
maybeParse = option Nothing . fmap pure

name :: Parser Name
name = do
    wb <- maybeParse (workbookName <* bang)
    ref <- nameStartCharacter <-> nameCharacters
    let name_ = Name wb ref
    pure name_ <?> "name"

nameCharacters :: Parser T.Text
nameCharacters = T.pack <$> many' nameCharacter <?> "nameCharacters"

nameStartCharacter :: Parser T.Text
nameStartCharacter = T.pack . pure <$> choice
    [ letter
    , underscore
    , backslash
    ] <?> "nameStartCharacter"

nameCharacter :: Parser Char
nameCharacter = choice
    [ letter
    , digit
    , underscore
    , fullStop
    ] <?> "nameCharacter"

--  =  <?> ""

-- | An example function.
main :: IO ()
main = do
    let query = "=SUM(Customers[ID])"
    let parsed = parseOnly formula query
    case parsed of
        Left str -> putStrLn str
        Right res -> putStrLn $ show res
