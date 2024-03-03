module Main (main) where

import Hsco

import System.Console.ANSI
import Data.Colour (Colour)
import Data.Colour.Names

import Control.Exception (bracket_)

import TextShow

import Data.Time

import Options.Applicative

withSGR :: [SGR] -> IO a -> IO a
withSGR sgr = bracket_ (setSGR sgr) (setSGR [Reset])

withRGBColor :: Colour Float -> IO a -> IO a
withRGBColor clr = withSGR [SetRGBColor Foreground clr]

data Command = Test | Save FilePath | Load FilePath

mainParser :: Parser Command
mainParser = subparser
    ( command "test" (info (pure Test) ( progDesc "Test" ))
   <> command "save" (info saveParser ( progDesc "Save" ))
   <> command "load" (info loadParser ( progDesc "Load" ))
    ) where
    saveParser = fmap Save $ argument str (metavar "FILE" <> action "file")
    loadParser = fmap Load $ argument str (metavar "FILE" <> action "file")

main :: IO ()
main = do
    cmd <- execParser $ info (mainParser <**> helper)
        ( fullDesc
       <> progDesc "TEST"
       <> header "hsco"
        )
    case cmd of
        Test -> testFunc
        Save fp -> saveFunc fp
        Load fp -> loadFunc fp

testFunc :: IO ()
testFunc = do
    timeZone <- getCurrentTimeZone
    curTime <- getCurrentTime
    print timeZone
    print curTime
    print $ utcToLocalTime timeZone curTime
    
    stdoutSupportsANSI <- hNowSupportsANSI stdout
    if stdoutSupportsANSI
        then do
            withRGBColor aquamarine $ printT $ defItem
            -- withRGBColor aquamarine $ putStr "Enter your name: "
            -- name <- withRGBColor orange $ hFlush stdout >> getLine
            -- withRGBColor lightblue $ putTextLn $ "Hello, " <> name <> "!"
        else
            putTextLn "Standard output does not support 'ANSI' escape codes."

saveFunc :: FilePath -> IO ()
saveFunc fp = todoSave fp defList

loadFunc :: FilePath -> IO ()
loadFunc fp = todoLoad fp >>= print
