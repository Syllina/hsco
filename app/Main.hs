module Main (main) where

import Hsco

import System.Console.Haskeline
import Data.Text as T

main5 :: IO ()
main5 = withWebVPN getInput (sayHello >> learnHello) where
    getInput = runInputT defaultSettings $ do
        (Just username) <- getInputLine "请输入学号: "
        (Just password) <- getPassword Nothing "请输入密码: "
        pure (T.pack username, T.pack password)

main :: IO ()
main = main5
