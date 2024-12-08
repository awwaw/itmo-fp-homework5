{-# LANGUAGE DerivingVia #-}

module HW5.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Data.Set ( Set, member )
import HW5.Base
    ( HiAction(..),
      HiMonad(runAction),
      HiValue (..),
      HiAction(..),
      HiMonad(runAction) )
import Control.Monad.Trans.Reader
import Data.Time (getCurrentTime)
import System.Random (getStdRandom, randomR)

import Control.Exception (Exception)
import Control.Exception.Base (throwIO)
import Data.ByteString (writeFile)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import Prelude hiding (writeFile)
import Control.Monad.IO.Class

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Ord, Eq)

data PermissionException =
  PermissionRequired HiPermission
  deriving (Show, Ord, Eq)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
    deriving (Functor, Applicative, Monad)
    via      (ReaderT (Set HiPermission) IO)

randomInRange :: (Int, Int) -> IO Int
randomInRange range = getStdRandom (randomR range)

instance HiMonad HIO where
  runAction (HiActionRead file) =
    runWithPermissions (Just AllowRead) $ do
      isDir <- doesDirectoryExist file
      if isDir
        then do
          contents <- listDirectory file
          let valueList = HiValueList . Seq.fromList . map (HiValueString . T.pack) $ contents
          return valueList
        else do
          fileContent <- readFile file
          return $ HiValueString (T.pack fileContent)

  runAction (HiActionWrite file text) =
    runWithPermissions (Just AllowWrite) $ do
      writeFile file text
      return HiValueNull

  runAction (HiActionChDir dir) =
    runWithPermissions (Just AllowRead) $ do
      setCurrentDirectory dir
      return HiValueNull

  runAction (HiActionMkDir dir) =
    runWithPermissions (Just AllowWrite) $ do
      createDirectory dir
      return HiValueNull

  runAction HiActionCwd =
    runWithPermissions (Just AllowRead) $ do
      HiValueString . T.pack <$> getCurrentDirectory

  runAction HiActionNow =
    runWithPermissions (Just AllowTime) $ do
      currentTime <- liftIO getCurrentTime
      return $ HiValueTime currentTime

  runAction (HiActionRand low high) =
    runWithPermissions Nothing $ do
      randomNum <- liftIO $ randomInRange (low, high)
      let randomRational = toRational randomNum
      return $ HiValueNumber randomRational

  runAction (HiActionEcho text) = runWithPermissions (Just AllowWrite) $ putStrLn (T.unpack text) >> pure HiValueNull


runWithPermissions :: Maybe HiPermission -> IO HiValue -> HIO HiValue
runWithPermissions permission action =
  HIO $ \permissions ->
    if isNothing permission || member (fromJust permission) permissions then action else throwIO $ PermissionRequired (fromJust permission)