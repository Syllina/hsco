module Main (main) where

import Hsco

import System.Console.Haskeline

main5 :: IO ()
main5 = withWebVPN getInput (sayHello >> learnHello) where
    getInput = runInputT defaultSettings $ do
        (Just username) <- getInputLine "请输入学号: "
        (Just password) <- getPassword Nothing "请输入密码: "
        pure (username, password)

main :: IO ()
main = main5
