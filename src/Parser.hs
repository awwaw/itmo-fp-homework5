module HW5.Parser
  ( parse
  ) where

import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Applicative (optional, empty, (<|>), some)
import Control.Monad (void)
import Data.ByteString (pack)
import qualified Data.Text as T

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.String (IsString(fromString))

-- import Data.Maybe (fromMaybe)


import Text.Megaparsec
  ( ParseErrorBundle,
    Parsec,
    choice,
    -- sepBy,
    runParser,
    MonadParsec (eof), sepBy,
    notFollowedBy,
    between,
    try,
    manyTill,
    many
  )

import HW5.Base (HiExpr (..), HiFun (..), HiValue (..), HiAction (..))

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (skipWhitespaces *> parseExpr <* eof) ""

------------------ Helper Functions ----------------------

skipWhitespaces :: Parser ()
skipWhitespaces = L.space space1 empty empty

myLexeme :: Parser a -> Parser a
myLexeme = L.lexeme skipWhitespaces

parseFun :: Parser HiValue
parseFun = myLexeme $ choice
  [ HiValueFunction HiFunAdd            <$ string "add",
    HiValueFunction HiFunDiv            <$ string "div",
    HiValueFunction HiFunMul            <$ string "mul",
    HiValueFunction HiFunSub            <$ string "sub",
    HiValueFunction HiFunNotEquals      <$ string "not-equals",
    HiValueFunction HiFunNotLessThan    <$ string "not-less-than",
    HiValueFunction HiFunNotGreaterThan <$ string "not-greater-than",
    HiValueFunction HiFunNot            <$ string "not",
    HiValueFunction HiFunAnd            <$ string "and",
    HiValueFunction HiFunOr             <$ string "or",
    HiValueFunction HiFunLessThan       <$ string "less-than",
    HiValueFunction HiFunGreaterThan    <$ string "greater-than",
    HiValueFunction HiFunEquals         <$ string "equals",
    HiValueFunction HiFunIf             <$ string "if",
    HiValueFunction HiFunToLower        <$ string "to-lower",
    HiValueFunction HiFunToUpper        <$ string "to-upper",
    HiValueFunction HiFunLength         <$ string "length",
    HiValueFunction HiFunTrim           <$ string "trim",
    HiValueFunction HiFunToLower        <$ string "to-lower",
    HiValueFunction HiFunToUpper        <$ string "to-upper",
    HiValueFunction HiFunReverse        <$ string "reverse",
    HiValueFunction HiFunList           <$ string "list",
    HiValueFunction HiFunFold           <$ string "fold",
    HiValueFunction HiFunRange          <$ string "range",
    HiValueFunction HiFunPackBytes      <$ string "pack-bytes",
    HiValueFunction HiFunUnpackBytes    <$ string "unpack-bytes",
    HiValueFunction HiFunEncodeUtf8     <$ string "encode-utf8",
    HiValueFunction HiFunDecodeUtf8     <$ string "decode-utf8",
    HiValueFunction HiFunZip            <$ string "zip",
    HiValueFunction HiFunUnzip          <$ string "unzip",
    HiValueFunction HiFunSerialise      <$ string "serialise",
    HiValueFunction HiFunDeserialise    <$ string "deserialise",
    HiValueFunction HiFunRead           <$ string "read",
    HiValueFunction HiFunWrite          <$ string "write",
    HiValueFunction HiFunMkDir          <$ string "mkdir",
    HiValueFunction HiFunChDir          <$ string "cd",
    HiValueFunction HiFunParseTime      <$ string "parse-time",
    HiValueFunction HiFunRand           <$ string "rand",
    HiValueFunction HiFunEcho           <$ string "echo",
    HiValueFunction HiFunCount          <$ string "count",
    HiValueFunction HiFunKeys           <$ string "keys",
    HiValueFunction HiFunValues         <$ string "values",
    HiValueFunction HiFunInvert         <$ string "invert"
  ]

parseMapEntrySet :: Parser [(HiExpr, HiExpr)]
parseMapEntrySet = skipWhitespaces *> parseMapEntry `sepBy` skipWhitespacesAndChar ','

parseMapEntry :: Parser (HiExpr, HiExpr)
parseMapEntry = do
  key <- parseExpr
  skipWhitespacesAndChar ':'
  value <- parseExpr
  return (key, value)

parseMap :: Parser HiExpr
parseMap = do
  entrySet <- between (skipWhitespacesAndChar '{') (skipWhitespacesAndChar '}') parseMapEntrySet
  skipWhitespaces
  return $ HiExprDict entrySet

parseCwd :: Parser HiValue
parseCwd = do
  _ <- skipWhitespaces *> string "cwd" <* skipWhitespaces
  return (HiValueAction HiActionCwd)

parseNow :: Parser HiValue
parseNow = do
  _ <- skipWhitespaces *> string "now" <* skipWhitespaces
  return (HiValueAction HiActionNow)

parseList :: Parser HiExpr
parseList = myLexeme $ do
  listElements <- between (string "[") (string "]") (parseExpr `sepBy` char ',')
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) listElements

parseByteStringLiteral :: Parser String
parseByteStringLiteral = string "[#" *> manyTill L.charLiteral (string "#]")

parseByteString :: Parser HiValue
parseByteString = do
  byteStr <- parseByteStringLiteral
  let bytes = map (\byte -> read ("0x" ++ byte)) (words byteStr)
  return $ HiValueBytes (pack bytes)

parseNumber :: Parser HiValue
parseNumber = do
  skipWhitespaces
  number <- L.signed skipWhitespaces L.scientific
  let rationalNum = toRational number
  return $ HiValueNumber rationalNum

parseValueExpr :: Parser HiExpr
parseValueExpr = HiExprValue <$> choice
  [
    parseFun,
    parseNumber,
    parseBool,
    parseNull,
    parseStringLiteral,
    parseByteString,
    parseCwd,
    parseNow
  ]

parseNull :: Parser HiValue
parseNull = myLexeme (HiValueNull <$ string "null")

parseStringLiteral :: Parser HiValue
parseStringLiteral = myLexeme $ do
  str <- char '"' *> manyTill L.charLiteral (char '"')
  return $ HiValueString (fromString str)

parseParens :: Char -> Parser HiExpr -> Parser HiExpr
parseParens parenType parser =
  if parenType == '(' then
    between (skipWhitespacesAndChar '(') (skipWhitespacesAndChar ')') parser
  else
    between (skipWhitespacesAndChar '[') (skipWhitespacesAndChar ']') parser

parseBool :: Parser HiValue
parseBool = myLexeme $ choice
  [
    HiValueBool True  <$ string "true",
    HiValueBool False <$ string "false"
  ]

skipWhitespacesAndChar :: Char -> Parser ()
skipWhitespacesAndChar c = skipWhitespaces *> void (char c) <* skipWhitespaces

skipWhitespacesAndString :: String -> Parser ()
skipWhitespacesAndString c = skipWhitespaces *> void (string c) <* skipWhitespaces

parseFieldAccess :: Parser HiExpr
parseFieldAccess = do
  _ <- char '.'
  fieldName <- some alphaNumChar
  return (HiExprValue (HiValueString (T.pack fieldName)))

parseFieldAccessChain :: Parser HiExpr -> Parser HiExpr
parseFieldAccessChain baseParser = do
  initial <- baseParser
  fields <- many (try parseFieldAccess)
  return $ foldl HiExprApply initial (map (:[]) fields)

parseTerm :: Parser HiExpr
parseTerm = do
  skipWhitespaces
  baseExpr <- parseValueExpr <|> parseList <|> parseMap
  appliedExpr <- parseApplyExpr baseExpr
  fieldAccessExpr <- parseFieldAccessChain (return appliedExpr)
  parseRunExpr (return fieldAccessExpr)


parseExpr :: Parser HiExpr
parseExpr = skipWhitespaces *> makeExprParser (choice [parseParens '(' parseExpr, parseTerm]) operatorTable <* skipWhitespaces

parseRunExpr :: Parser HiExpr -> Parser HiExpr
parseRunExpr p = do
  expr    <- p
  postfix <- optional (try (skipWhitespacesAndChar '!'))
  case postfix of
    Just _  -> return (HiExprRun expr)
    Nothing -> return expr

parseApplyExpr :: HiExpr -> Parser HiExpr
parseApplyExpr funExpr = do
  skipWhitespaces
  fun <- optional $ do
    skipWhitespacesAndChar '('
    args <- parseExpr `sepBy` skipWhitespacesAndChar ','
    skipWhitespacesAndChar ')'
    return args
  skipWhitespaces
  case fun of
    Just args -> parseApplyExpr (HiExprApply funExpr args)
    Nothing   -> return funExpr

getFunction :: HiFun -> HiExpr -> HiExpr -> HiExpr
getFunction f l r = HiExprApply (HiExprValue (HiValueFunction f)) [l, r]

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [
    [
      InfixL $ getFunction HiFunMul <$ skipWhitespacesAndString "*",
      InfixL (getFunction HiFunDiv <$ try (myLexeme (string "/" <* notFollowedBy (string "="))))
    ],
    [
      InfixL $ getFunction HiFunAdd <$ skipWhitespacesAndString "+",
      InfixL $ getFunction HiFunSub <$ skipWhitespacesAndString "-"
    ],
    [
      InfixN $ getFunction HiFunNotLessThan <$ skipWhitespacesAndString ">=",
      InfixN $ getFunction HiFunNotGreaterThan <$ skipWhitespacesAndString "<=",
      InfixN $ getFunction HiFunLessThan <$ myLexeme (string "<" <* notFollowedBy (string "=")),
      InfixN $ getFunction HiFunGreaterThan <$ myLexeme (string ">" <* notFollowedBy (string "=")),
      InfixN $ getFunction HiFunEquals <$ skipWhitespacesAndString "==",
      InfixN $ getFunction HiFunNotEquals <$ skipWhitespacesAndString "/="
    ],
    [
      InfixR $ getFunction HiFunAnd <$ skipWhitespacesAndString "&&"
    ],
    [
      InfixR $ getFunction HiFunOr <$ skipWhitespacesAndString "||"
    ]
  ]
