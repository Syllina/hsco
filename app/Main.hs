module Main (main) where

import Hsco

import System.Console.ANSI
import Data.Colour (Colour)
import Data.Colour.Names

import Control.Exception (bracket_)

import TextShow

import Data.Time

import Options.Applicative

import qualified Data.Text as T

withSGR :: [SGR] -> IO a -> IO a
withSGR sgr = bracket_ (setSGR sgr) (setSGR [Reset])

withRGBColor :: Colour Float -> IO a -> IO a
withRGBColor clr = withSGR [SetRGBColor Foreground clr]

data Command = Test | Save FilePath | Load FilePath | Add FilePath | Create FilePath

mainParser :: Parser Command
mainParser = subparser
    ( command "test" (info (pure Test) ( progDesc "Test" ))
   <> command "save" (info saveParser ( progDesc "Save" ))
   <> command "load" (info loadParser ( progDesc "Load" ))
   <> command "add" (info addParser ( progDesc "Add" ))
   <> command "create" (info createParser ( progDesc "Create" ))
    ) where
    saveParser = fmap Save $ argument str (metavar "FILE" <> action "file")
    loadParser = fmap Load $ argument str (metavar "FILE" <> action "file")
    addParser = fmap Add $ argument str (metavar "FILE" <> action "file")
    createParser = fmap Create $ argument str (metavar "FILE" <> action "file")

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
        Add fp -> addFunc fp
        Create fp -> createFunc fp

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
loadFunc fp = todoLoad fp >>= printT

addFunc :: FilePath -> IO ()
addFunc fp = do
    title <- askLine "Title: "
    description <- fmap strMaybe $ askLine "Description (optional): "
    -- deadline <- askLine "Deadline: "
    deadline <- pure Nothing
    tags <- fmap (\x -> [x]) $ fmap Tag $ fmap words $ askLine "Tags: "
    putTextLn "Priority:"
    urgency <- fmap (readMaybe . T.unpack) $ askLine "  Urgency: " :: IO (Maybe Int)
    importance <- fmap (readMaybe . T.unpack) $ askLine "  Importance: " :: IO (Maybe Int)
    let priority = Priority <$> urgency <*> importance
    let item = fmap (TodoItem title description deadline tags) priority
    list <- todoLoad fp
    let action = do
            list' <- list
            item' <- item
            pure $ do
                putTextLn $ "Add item" <> showt item'
                todoSave fp $ list' <> TodoList [item']
    maybe (pure ()) id action
    where
        askLine st = putText st >> hFlush stdout >> getLine
        strMaybe st = T.strip st & \st -> if T.null st then Nothing else Just st

createFunc :: FilePath -> IO ()
createFunc fp = todoSave fp mempty
