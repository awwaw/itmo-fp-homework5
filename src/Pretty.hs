module HW5.Pretty
  ( prettyValue
  , prettyError
  ) where

import Prettyprinter (Doc, pretty, annotate, viaShow, encloseSep, (<+>))
import Prettyprinter.Render.Terminal
    ( AnsiStyle, Color(..), color, colorDull )
import Data.Ratio (denominator, numerator)
import Data.Scientific
    ( fromRationalRepetendUnlimited, formatScientific, FPFormat(..) )

import HW5.Base (HiValue (..), HiError (..), HiFun (..), HiAction (..))
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.ByteString (unpack, ByteString)
import Numeric (showHex)
import Data.Map (toList)


toLowerString :: [Char] -> [Char]
toLowerString = map toLower

byteArrayToString :: ByteString -> String
byteArrayToString bytes = unwords $ map (\n -> let s = showHex n "" in if length s == 1 then "0" <> s else s) (unpack bytes)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber number) =
  if denominator number == 1
    then
      annotate (color Magenta) (pretty (numerator number))
    else
      case fromRationalRepetendUnlimited number of
        (val, Nothing) -> annotate (color Magenta) (pretty (formatScientific Fixed Nothing val))
        _              -> annotate (color Magenta) (pretty (prettyIrrational number))
prettyValue (HiValueBool boolValue) = annotate (color Cyan) $ pretty (toLowerString $ show boolValue)
prettyValue (HiValueFunction func)  = annotate (colorDull Yellow) $ pretty (funName func)
prettyValue (HiValueString str)     = annotate (color Green) $ pretty (show str)
prettyValue HiValueNull             = annotate (colorDull Blue) $ pretty "null"
prettyValue (HiValueList s)         = encloseSep (pretty "[") (pretty "]") (pretty ", ") (prettyValue <$> Data.Foldable.toList s)
prettyValue (HiValueBytes bytes)    = annotate (colorDull Magenta) (pretty "[# ")
                                      <> pretty (byteArrayToString bytes)
                                      <> pretty " #]"
prettyValue (HiValueAction action)  = prettyAction action
prettyValue (HiValueTime time)      = annotate (color Magenta) $ viaShow time
prettyValue (HiValueDict dict)      = encloseSep (pretty "{") (pretty "}") (pretty ", ") (map formatPair (Data.Map.toList dict))
  where
    formatPair (key, value) = prettyValue key <+> pretty ":" <+> prettyValue value


prettyApplyFunction :: HiFun -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyApplyFunction func [arg]        = annotate (color Yellow) $ pretty (funName func) <> pretty "(" <> arg <> pretty ")"
prettyApplyFunction func [arg1, arg2] = annotate (color Yellow) $ pretty (funName func) <> pretty "(" <> arg1 <> pretty ", " <> arg2 <> pretty ")"
prettyApplyFunction _ _               = undefined

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction (HiActionRead path)        = prettyApplyFunction HiFunRead  [viaShow path]
prettyAction (HiActionWrite path bytes) = prettyApplyFunction HiFunWrite [viaShow path, prettyValue (HiValueBytes bytes)]
prettyAction (HiActionMkDir path)       = prettyApplyFunction HiFunMkDir [viaShow path]
prettyAction (HiActionChDir path)       = prettyApplyFunction HiFunChDir [viaShow path]
prettyAction (HiActionRand low high)    = prettyApplyFunction HiFunRand [prettyValue ((HiValueNumber . toRational) low), prettyValue ((HiValueNumber . toRational) high)]
prettyAction HiActionNow                = annotate (color Yellow) $ pretty "now"
prettyAction HiActionCwd                = annotate (color Yellow) $ pretty "cwd"
prettyAction (HiActionEcho text)        = prettyApplyFunction HiFunEcho [pretty text]

funName :: HiFun -> [Char]
funName func = case func of
  HiFunDiv            -> "div"
  HiFunMul            -> "mul"
  HiFunSub            -> "sub"
  HiFunAdd            -> "add"
  HiFunNot            -> "not"
  HiFunAnd            -> "and"
  HiFunOr             -> "or"
  HiFunLessThan       -> "less-than"
  HiFunGreaterThan    -> "greater-than"
  HiFunEquals         -> "equals"
  HiFunNotLessThan    -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals      -> "not-equals"
  HiFunIf             -> "if"
  HiFunLength         -> "length"
  HiFunToLower        -> "to-lower"
  HiFunToUpper        -> "to-upper"
  HiFunReverse        -> "reverse"
  HiFunTrim           -> "trim"
  HiFunList           -> "list"
  HiFunRange          -> "range"
  HiFunFold           -> "fold"
  HiFunPackBytes      -> "pack-bytes"
  HiFunUnpackBytes    -> "unpack-bytes"
  HiFunZip            -> "zip"
  HiFunUnzip          -> "unzip"
  HiFunEncodeUtf8     -> "encode-utf8"
  HiFunDecodeUtf8     -> "decode-utf8"
  HiFunSerialise      -> "serialise"
  HiFunDeserialise    -> "deserialise"
  HiFunRead           -> "read"
  HiFunWrite          -> "write"
  HiFunMkDir          -> "mkdir"
  HiFunChDir          -> "cd"
  HiFunParseTime      -> "parse-time"
  HiFunRand           -> "rand"
  HiFunEcho           -> "echo"
  HiFunCount          -> "count"
  HiFunInvert         -> "invert"
  HiFunKeys           -> "keys"
  HiFunValues         -> "values"

prettyIrrational :: Rational -> String
prettyIrrational val =
    if absIntPart <= 1
    then show numeratorVal ++ "/" ++ show denominatorVal
    else
        let fractionPart = prettyFraction denominatorVal (quotRem (abs numeratorVal) denominatorVal)
        in if intPart < 0
           then show (intPart + 1) ++ " - " ++ fractionPart
           else show intPart ++ " + " ++ fractionPart
  where
    numeratorVal = numerator val
    denominatorVal = denominator val
    intPart = div numeratorVal denominatorVal
    absIntPart = abs intPart


prettyFraction :: Integer -> (Integer, Integer) -> String
prettyFraction num (_, denom) = show denom ++ "/" ++ show num

prettyError :: HiError -> Doc AnsiStyle
prettyError e = annotate (color Red) $ viaShow e