module Main (main) where

import Control.Monad.IO.Class
import HW5.Evaluator
import HW5.Parser
import HW5.Pretty
import HW5.Action
import Prettyprinter.Render.Terminal (putDoc)
import Data.Set (Set, fromList)


import System.Console.Haskeline
import Text.Megaparsec (errorBundlePretty)

permissions :: Set HiPermission
permissions = fromList [AllowRead, AllowWrite, AllowTime]

mainLoop :: InputT IO()
mainLoop = do
    inputLine <- getInputLine "hi> "
    case inputLine of
        Nothing     -> return ()
        Just "exit" -> return ()
        Just inp    -> do
            processInput inp
            mainLoop

processInput :: String -> InputT IO ()
processInput line = 
    case parse line of
        Left err         -> liftIO $ putStrLn $ errorBundlePretty err
        Right expression -> do
            result <- liftIO $ runHIO (eval expression) permissions
            case result of
                Left err    -> do
                    liftIO $ putDoc $ prettyError err
                    liftIO $ putStrLn ""
                Right value -> do
                    liftIO $ putDoc $ prettyValue value
                    liftIO $ putStrLn ""

main :: IO ()
main = runInputT defaultSettings mainLoop
