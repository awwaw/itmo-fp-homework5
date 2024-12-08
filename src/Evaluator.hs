module HW5.Evaluator
  ( eval
  ) where

import HW5.Base (HiFun (..), HiError (..), HiExpr (..), HiValue (..), HiMonad (..), HiAction (..))

import Data.Word (Word8)
-- import Data.Map ( Map, findWithDefault, keys, elems)

import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Clock (addUTCTime)

import qualified Data.Sequence as S
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
import Data.ByteString.Lazy (fromStrict, toStrict)
import Codec.Compression.Zlib
import Codec.Serialise (deserialise, serialise)
import Data.Foldable (toList)
import Control.Monad.Trans.Class (lift)

import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import qualified Data.Text as T
import Data.Ratio (denominator, numerator, (%))
import Data.Semigroup (Semigroup(stimes))
import Control.Monad (foldM, unless)
import Text.Read (readMaybe)
import qualified Data.Map as Map


type HiExcept m a = ExceptT HiError m a

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExpression

evalExpression :: HiMonad m => HiExpr -> HiExcept m HiValue
evalExpression (HiExprValue val)    = pure val
evalExpression (HiExprDict d) = do
  evaluatedPairs <- mapM (\(x, y) -> do
    p <- evalExpression x
    q <- evalExpression y
    return (p, q)) d
  return $ HiValueDict (Map.fromList evaluatedPairs)
evalExpression (HiExprApply f args) = do
  evaluatedF <- evalExpression f
  case evaluatedF of
    HiValueFunction HiFunAnd -> myAnd args
    HiValueFunction HiFunOr  -> myOr args
    HiValueFunction HiFunIf  -> myIf args
    _                        -> evalApplication evaluatedF args
evalExpression (HiExprRun expr) = do
  ev <- evalExpression expr
  case ev of
    (HiValueAction evaledFunc) -> lift $ runAction evaledFunc
    _                          -> throwHiError HiErrorInvalidFunction

evalApplication :: HiMonad m => HiValue -> [HiExpr] -> HiExcept m HiValue
evalApplication f x = do
  -- Evaluating arguments
  val <- mapM evalExpression x
  case f of
    HiValueFunction func  -> applyFunction func val
    (HiValueString s)     -> strGetByIndex s val
    (HiValueList s)       -> listGetByIndex s val
    (HiValueBytes bytes)  -> bytesGetByIndex bytes val
    (HiValueDict dict)    -> mapGetOrDefault dict val
    _                     -> throwHiError HiErrorInvalidFunction

mapGetOrDefault :: HiMonad m => Map.Map HiValue HiValue -> [HiValue] -> HiExcept m HiValue
mapGetOrDefault dict [key] = return $ Map.findWithDefault HiValueNull key dict
mapGetOrDefault _    _     = throwHiError HiErrorInvalidArgument

binaryCmp :: HiMonad m => (HiValue -> HiValue -> Bool) -> [HiValue] -> HiExcept m HiValue
-- f is one of [(<), (==), (>), etc]
binaryCmp f [x, y] = return $ HiValueBool (f x y)
binaryCmp _  _     = throwHiError HiErrorArityMismatch

applyFunction :: HiMonad m => HiFun -> [HiValue] -> HiExcept m HiValue
--------- Arithmetic operations ----------
applyFunction HiFunAdd            = binaryFun add
applyFunction HiFunSub            = binaryFun sub
applyFunction HiFunMul            = binaryFun myMul
applyFunction HiFunDiv            = binaryFun myDiv
----------- Comp operations --------------
applyFunction HiFunEquals         = binaryCmp (==)
applyFunction HiFunNotEquals      = binaryCmp (/=)
applyFunction HiFunLessThan       = binaryCmp (<)
applyFunction HiFunGreaterThan    = binaryCmp (>)
applyFunction HiFunNotGreaterThan = binaryCmp (<=)
applyFunction HiFunNotLessThan    = binaryCmp (>=)
applyFunction HiFunNot            = unaryFun myNot
----------- String methods -----------
applyFunction HiFunLength         = unaryFun strlistlen
applyFunction HiFunToLower        = modifyString T.toLower
applyFunction HiFunToUpper        = modifyString T.toUpper
applyFunction HiFunReverse        = modifyContainer myReverse
applyFunction HiFunTrim           = modifyString T.strip
----------- List methods -----------
applyFunction HiFunList           = return . HiValueList . S.fromList
applyFunction HiFunFold           = binaryFun myFold
applyFunction HiFunRange          = myRange
----------- Bytes 'n shii -----------
applyFunction HiFunPackBytes      = myPackBytes
applyFunction HiFunUnpackBytes    = myUnpackBytes
applyFunction HiFunEncodeUtf8     = myEncodeUtf8
applyFunction HiFunDecodeUtf8     = myDecodeUtf8
applyFunction HiFunZip            = myZip
applyFunction HiFunUnzip          = myUnzip
applyFunction HiFunSerialise      = mySerialize
applyFunction HiFunDeserialise    = myDeserialize
----------- I/O -----------
applyFunction HiFunRead           = myRead
applyFunction HiFunWrite          = myWrite
applyFunction HiFunMkDir          = myMkDir
applyFunction HiFunChDir          = myChDir
----------- Time -----------
applyFunction HiFunParseTime      = myParseTime
----------- Gambling -----------
applyFunction HiFunRand           = myRand
----------- Echoing -----------
applyFunction HiFunEcho           = myEcho
----------- Dict Access -----------
applyFunction HiFunKeys           = unaryFun myKeys
applyFunction HiFunValues         = unaryFun myValues
applyFunction HiFunInvert         = unaryFun myInvert
applyFunction HiFunCount          = unaryFun myCount

applyFunction _                   = const $ throwHiError HiErrorInvalidArgument

----------- Dict Access -----------

myKeys :: HiMonad m => HiValue -> HiExcept m HiValue
myKeys (HiValueDict dict) = return $ HiValueList $ S.fromList $ Map.keys dict
myKeys _                  = throwHiError HiErrorInvalidArgument

myValues :: HiMonad m => HiValue -> HiExcept m HiValue
myValues (HiValueDict dict) = return $ HiValueList $ S.fromList $ Map.elems dict
myValues _                  = throwHiError HiErrorInvalidArgument

myInvert :: HiMonad m => HiValue -> HiExcept m HiValue
myInvert (HiValueDict dict) = return $ HiValueDict invertedDict
  where
    valueToKeysList = Map.foldrWithKey collectKeys Map.empty dict

    collectKeys :: HiValue -> HiValue -> Map.Map HiValue [HiValue] -> Map.Map HiValue [HiValue]
    collectKeys key value = Map.insertWith (++) value [key]

    invertedDict = Map.map (HiValueList . S.fromList) valueToKeysList
myInvert _ = throwHiError HiErrorInvalidArgument

countOccurrences :: [a] -> (a -> HiValue) -> HiValue
countOccurrences items transformer =
  HiValueDict $ foldl insertOrIncrement Map.empty items
  where 
    myAdd :: HiValue -> HiValue -> HiValue
    myAdd (HiValueNumber a) (HiValueNumber b) = HiValueNumber (a + b)
    myAdd _ _ = HiValueNumber 0
    
    insertOrIncrement acc item =
      Map.insertWith myAdd (transformer item) (HiValueNumber 1) acc


myCount :: HiMonad m => HiValue -> HiExcept m HiValue
myCount (HiValueList lst)    = return $ countOccurrences (toList lst) id
myCount (HiValueBytes bytes) = return $ countOccurrences (BS.unpack bytes) (HiValueNumber . fromIntegral)
myCount (HiValueString text) = return $ countOccurrences (T.unpack text) (HiValueString . T.singleton)
myCount _ = throwHiError HiErrorInvalidArgument

----------- Echoing -----------

myEcho :: HiMonad m => [HiValue] -> HiExcept m HiValue
myEcho [HiValueString text] = return $ HiValueAction $ HiActionEcho text
myEcho _                    = throwHiError HiErrorInvalidArgument

----------- Gambling -----------
myRand :: HiMonad m => [HiValue] -> HiExcept m HiValue
myRand [HiValueNumber low, HiValueNumber high] = return $ HiValueAction $ HiActionRand (round low) (round high)
myRand _                                       = throwHiError HiErrorInvalidArgument

----------- Time -----------

myParseTime :: HiMonad m => [HiValue] -> HiExcept m HiValue
myParseTime [HiValueString time] = return $ maybe HiValueNull HiValueTime (readMaybe (T.unpack time) :: Maybe UTCTime)
myParseTime _                    = throwHiError HiErrorInvalidArgument

----------- I/O -----------

unaryIOFunction :: HiMonad m => (FilePath -> HiAction) -> T.Text -> HiExcept m HiValue
unaryIOFunction constructor arg = return $ HiValueAction $ constructor (T.unpack arg)

myRead :: HiMonad m => [HiValue] -> HiExcept m HiValue
myRead [HiValueString filename] = unaryIOFunction HiActionRead filename
myRead _ = throwHiError HiErrorInvalidArgument

myWrite :: HiMonad m => [HiValue] -> HiExcept m HiValue
myWrite [HiValueString filename, HiValueString content] =
  return $ HiValueAction $ HiActionWrite (T.unpack filename) (E.encodeUtf8 content)
myWrite _ = throwHiError HiErrorInvalidArgument

myMkDir :: HiMonad m => [HiValue] -> HiExcept m HiValue
myMkDir [HiValueString directoryName] = unaryIOFunction HiActionMkDir directoryName
myMkDir _ = throwHiError HiErrorInvalidArgument

myChDir :: HiMonad m => [HiValue] -> HiExcept m HiValue
myChDir [HiValueString directoryName] = unaryIOFunction HiActionChDir directoryName
myChDir _ = throwHiError HiErrorInvalidArgument

----------- Bytes -----------
toByte :: HiMonad m => HiValue -> HiExcept m Word8
toByte (HiValueNumber n) =
  let byteValue = fromInteger (numerator n)
  in if denominator n == 1 && byteValue >= 0 && byteValue <= 255
      then return $ fromInteger byteValue
      else throwHiError HiErrorInvalidArgument
toByte _ = throwHiError HiErrorInvalidArgument

myPackBytes :: HiMonad m => [HiValue] -> HiExcept m HiValue
myPackBytes [HiValueList values] = do
  bytes <- mapM toByte (toList values)
  return $ HiValueBytes (BS.pack bytes)
myPackBytes _ = throwHiError HiErrorInvalidArgument

myUnpackBytes :: HiMonad m => [HiValue] -> HiExcept m HiValue
myUnpackBytes [HiValueBytes bs] = do
  let bytes  = BS.unpack bs
      values = map (\b -> HiValueNumber (fromIntegral b % 1)) bytes
  return $ HiValueList (S.fromList values)
myUnpackBytes _ = throwHiError HiErrorInvalidArgument

myEncodeUtf8 :: HiMonad m => [HiValue] -> HiExcept m HiValue
myEncodeUtf8 [HiValueString str] = do
  let bytes = E.encodeUtf8 str
  return $ HiValueBytes bytes
myEncodeUtf8 _ = throwHiError HiErrorInvalidArgument

myDecodeUtf8 :: HiMonad m => [HiValue] -> HiExcept m HiValue
myDecodeUtf8 [HiValueBytes bytes] =
  case E.decodeUtf8' bytes of
    Left _     -> return HiValueNull
    Right text -> return $ HiValueString text
myDecodeUtf8 _ = throwHiError HiErrorInvalidArgument

myZip :: HiMonad m => [HiValue] -> HiExcept m HiValue
myZip [HiValueBytes bytes] =
   return $ HiValueBytes $ toStrict $ compressWith (defaultCompressParams {compressLevel = bestCompression}) (fromStrict bytes)
myZip _ = throwHiError HiErrorInvalidArgument

myUnzip :: HiMonad m => [HiValue] -> HiExcept m HiValue
myUnzip [HiValueBytes bytes] =
  return $ HiValueBytes $ toStrict (decompressWith defaultDecompressParams (fromStrict bytes))
myUnzip _ = throwHiError HiErrorInvalidArgument

mySerialize :: HiMonad m => [HiValue] -> HiExcept m HiValue
mySerialize [value] = return $ HiValueBytes $ toStrict $ serialise value
mySerialize _       = throwHiError HiErrorArityMismatch

myDeserialize :: HiMonad m => [HiValue] -> HiExcept m HiValue
myDeserialize [HiValueBytes bytes] = return $ deserialise $ fromStrict bytes
myDeserialize _                    = throwHiError HiErrorInvalidArgument

----------- Logic functions -----------

myAnd :: HiMonad m => [HiExpr] -> HiExcept m HiValue
myAnd [a, b] = do
  leftEvaluated <- evalExpression a
  case leftEvaluated of
    (HiValueBool False) -> return leftEvaluated
    HiValueNull         -> return HiValueNull
    _                   -> evalExpression b
myAnd _ = throwHiError HiErrorArityMismatch

myOr :: HiMonad m => [HiExpr] -> HiExcept m HiValue
myOr [a, b] = do
  leftEvaluated <- evalExpression a
  case leftEvaluated of
    HiValueNull         -> evalExpression b
    (HiValueBool False) -> evalExpression b
    _                   -> return leftEvaluated
myOr _ = throwHiError HiErrorArityMismatch

myIf :: HiMonad m => [HiExpr] -> HiExcept m HiValue
myIf [condition, thenBranch, elseBranch] = do
  conditionEvaluated <- evalExpression condition
  case conditionEvaluated of
    (HiValueBool True)  -> evalExpression thenBranch
    (HiValueBool False) -> evalExpression elseBranch
    _                   -> throwHiError HiErrorInvalidArgument
myIf _ = throwHiError HiErrorArityMismatch

unaryFun :: HiMonad m => (HiValue -> HiExcept m HiValue) -> [HiValue] -> HiExcept m HiValue
unaryFun f [x] = f x
unaryFun _  _  = throwHiError HiErrorArityMismatch

myNot :: HiMonad m => HiValue -> HiExcept m HiValue
myNot (HiValueBool b) = return $ HiValueBool (not b)
myNot _ = throwHiError HiErrorInvalidArgument

----------- Arithmetic functions -----------

binaryFun :: HiMonad m => (HiValue -> HiValue -> HiExcept m HiValue) -> [HiValue] -> HiExcept m HiValue
binaryFun f [x, y] = f x y
binaryFun _ _      = throwHiError HiErrorArityMismatch

add :: HiMonad m => HiValue -> HiValue -> HiExcept m HiValue
add (HiValueNumber x) (HiValueNumber y)       = return $ HiValueNumber (x + y)
add (HiValueString x) (HiValueString y)       = return $ HiValueString (x <> y)
add (HiValueList x) (HiValueList y)           = return $ HiValueList (x <> y)
add (HiValueBytes x) (HiValueBytes y)         = return $ HiValueBytes (x <> y)
add (HiValueTime time) (HiValueNumber delta) = return $ HiValueTime $ addUTCTime (fromRational delta) time
add _                  _                      = throwHiError HiErrorInvalidArgument

sub :: HiMonad m => HiValue -> HiValue -> HiExcept m HiValue
sub (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x - y)
sub (HiValueTime t1) (HiValueTime t2)   = return $ HiValueNumber (toRational (diffUTCTime t1 t2))
sub _                  _                = throwHiError HiErrorInvalidArgument

expectNonNegativeInteger :: HiMonad m => Rational -> HiExcept m Int
expectNonNegativeInteger y = do
  unless (y >= 0 && denominator y == 1) $ throwHiError HiErrorInvalidArgument
  return $ fromInteger (numerator y)

myMul :: HiMonad m => HiValue -> HiValue -> HiExcept m HiValue
myMul (HiValueNumber x) (HiValueNumber y) = return $ HiValueNumber (x * y)
myMul (HiValueString x) (HiValueNumber y) = do
  howmuch <- expectNonNegativeInteger y
  return $ HiValueString (stimes howmuch x)
myMul (HiValueList x) (HiValueNumber y)   = do
  howmuch <- expectNonNegativeInteger y
  return $ HiValueList (stimes howmuch x)
myMul (HiValueBytes x) (HiValueNumber y)  = do
  howmuch <- expectNonNegativeInteger y
  return $ HiValueBytes (stimes howmuch x)
myMul _ _ = throwHiError HiErrorInvalidArgument

myDiv :: HiMonad m => HiValue -> HiValue -> HiExcept m HiValue
myDiv (HiValueNumber x) (HiValueNumber y)
  | y == 0    = throwHiError HiErrorDivideByZero
  | otherwise = return $ HiValueNumber (x / y)
myDiv (HiValueString x) (HiValueString y) = return $ HiValueString $ x <> T.singleton '/' <> y
myDiv _ _ = throwHiError HiErrorInvalidArgument

----------- String methods -----------

strlistlen :: HiMonad m => HiValue -> HiExcept m HiValue
strlistlen (HiValueString str) = return $ HiValueNumber (toRational $ T.length str)
strlistlen (HiValueList lst)   = return $ HiValueNumber (toRational $ S.length lst)
strlistlen _                       = throwHiError HiErrorInvalidArgument

type StringModification = (T.Text -> T.Text)

myReverse :: HiValue -> HiValue
myReverse (HiValueString str)  = HiValueString (T.reverse str)
myReverse (HiValueList s)    = HiValueList (S.reverse s)
myReverse _                    = undefined

-- [HiValue] is used for `unaryFun` compatibility
-- Makes `toLower` and `toUpper`
modifyContainer :: HiMonad m => (HiValue -> HiValue) -> [HiValue] -> HiExcept m HiValue
modifyContainer modif = unaryFun $ applyContainerModification modif

applyContainerModification :: HiMonad m => (HiValue -> HiValue) -> HiValue -> HiExcept m HiValue
applyContainerModification f s = case s of
  (HiValueString _) -> return $ f s
  (HiValueList _)   -> return $ f s
  _                   -> throwHiError HiErrorInvalidArgument

modifyString :: HiMonad m => StringModification -> [HiValue] -> HiExcept m HiValue
modifyString modif = unaryFun $ applyStringModification modif

applyStringModification :: HiMonad m => StringModification -> HiValue -> HiExcept m HiValue
applyStringModification f s = case s of
  (HiValueString str) -> return $ HiValueString $ f str
  _                   -> throwHiError HiErrorInvalidArgument


-- String indexes --
recalculateIndex :: HiValue -> Int -> Bool -> Int
recalculateIndex x len isStart = case x of
  HiValueNumber n ->
    let newIndex = fromInteger (numerator n) in
      if newIndex < 0 then len + newIndex else newIndex
  HiValueNull     -> if isStart then 0 else len -- Начало для start
  _               -> -1

getByIndex ::
    (HiMonad m) =>
    (s -> Int) ->       -- .length()
    (s -> Int -> a) ->  -- .get(idx)
    (Int -> s -> s) ->  -- .drop(n)
    (Int -> s -> s) ->  -- .take(n)
    (a -> HiValue) ->   -- Get container from single element
    (s -> HiValue) ->   -- Get container from slice
    s -> [HiValue] -> HiExcept m HiValue
getByIndex lenFn indexFn dropFn takeFn wrapElemFn wrapSeqFn sequ inds = case inds of
    [HiValueNumber index] -> handleIndex index
    [start, end] -> handleSlice start end
    _ -> throwHiError HiErrorInvalidArgument
  where
    seqLength = lenFn sequ

    handleIndex index = do
      if denominator index /= 1
        then throwHiError HiErrorInvalidArgument
        else do
          let ind = fromInteger (numerator index) :: Int
          if ind < 0 || ind >= seqLength
            then return HiValueNull
            else return $ wrapElemFn $ indexFn sequ ind

    handleSlice start end = do
        let startInd = recalculateIndex start seqLength True
            endInd   = recalculateIndex end seqLength False

        if startInd == -1 || endInd == -1
          then throwHiError HiErrorInvalidArgument
          else do
            let s = max 0 (min seqLength startInd)
                e = max 0 (min seqLength endInd)
                start' = min s e
                end'   = max s e

            let sliceSeq = takeFn (end' - start') (dropFn start' sequ)
            return $ wrapSeqFn sliceSeq

strGetByIndex :: HiMonad m => T.Text -> [HiValue] -> HiExcept m HiValue
strGetByIndex = getByIndex T.length T.index T.drop T.take (HiValueString . T.singleton) HiValueString

listGetByIndex :: HiMonad m => S.Seq HiValue -> [HiValue] -> HiExcept m HiValue
listGetByIndex = getByIndex S.length S.index S.drop S.take id HiValueList

bytesGetByIndex :: HiMonad m => BS.ByteString -> [HiValue] -> HiExcept m HiValue
bytesGetByIndex = getByIndex BS.length BS.index BS.drop BS.take (HiValueNumber . fromIntegral) HiValueBytes

----------- List methods -----------

myFold :: HiMonad m => HiValue -> HiValue -> HiExcept m HiValue
myFold (HiValueFunction function) (HiValueList (h S.:<| t)) =
  foldM (\el1 el2 -> applyFunction function [el1, el2]) h t
myFold _ _ = throwHiError HiErrorInvalidArgument

myRange :: HiMonad m => [HiValue] -> HiExcept m HiValue
myRange [HiValueNumber from, HiValueNumber to] = return $ HiValueList (HiValueNumber <$> S.fromList [from..to])
myRange _ = throwHiError HiErrorInvalidArgument

--------------------- Helper Functions -------------------------------

throwHiError :: HiMonad m => HiError -> HiExcept m a
throwHiError = throwE