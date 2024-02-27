module Main (main) where

import Hsco

import System.Console.ANSI
import Data.Colour (Colour)
import Data.Colour.Names

import Control.Exception (bracket_)

import TextShow

withSGR :: [SGR] -> IO a -> IO a
withSGR sgr = bracket_ (setSGR sgr) (setSGR [Reset])

withRGBColor :: Colour Float -> IO a -> IO a
withRGBColor clr = withSGR [SetRGBColor Foreground clr]

main :: IO ()
main = do
    -- todoSave "todolist.db" defList
    list <- todoLoad "todolist.db"
    print list
    stdoutSupportsANSI <- hNowSupportsANSI stdout
    if stdoutSupportsANSI
        then do
            withRGBColor aquamarine $ printT $ defItem
            -- withRGBColor aquamarine $ putStr "Enter your name: "
            -- name <- withRGBColor orange $ hFlush stdout >> getLine
            -- withRGBColor lightblue $ putTextLn $ "Hello, " <> name <> "!"
        else
            putTextLn "Standard output does not support 'ANSI' escape codes."
