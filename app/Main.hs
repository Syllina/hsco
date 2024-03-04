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

data TodoCommand = Save FilePath | Load FilePath | Add FilePath | Create FilePath

data Command = Todo TodoCommand | Test

mainParser :: Parser Command
mainParser = subparser
    ( command "todo" (info (fmap Todo todoParser) ( progDesc "TODO manager" ))
   <> command "test" (info (pure Test) ( progDesc "debug" ))
    )

todoParser :: Parser TodoCommand
todoParser = subparser
    ( command "save" (info saveParser ( progDesc "Save" ))
   <> command "load" (info loadParser ( progDesc "Load" ))
   <> command "add" (info addParser ( progDesc "Add" ))
   <> command "create" (info createParser ( progDesc "Create" ))
    ) where
    saveParser = fmap Save $ argument str (metavar "FILE" <> action "file")
    loadParser = fmap Load $ argument str (metavar "FILE" <> value "todo.dat" <> action "file")
    addParser = fmap Add $ argument str (metavar "FILE" <> value "todo.dat" <> action "file")
    createParser = fmap Create $ argument str (metavar "FILE" <> value "todo.dat" <> action "file")

main :: IO ()
main = do
    cmd <- execParser $ info (mainParser <**> helper)
        ( fullDesc
       <> progDesc "hsco hsco hsco hsco"
       <> header "hsco"
        )
    case cmd of
        Test -> testFunc
        Todo (Save fp) -> saveFunc fp
        Todo (Load fp) -> loadFunc fp
        Todo (Add fp) -> addFunc fp
        Todo (Create fp) -> createFunc fp

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
-- todoLoad fp :: IO (Maybe TodoList)
loadFunc fp = todoLoad fp >>= maybe (putTextLn "load error") printT

addFunc :: FilePath -> IO ()
addFunc fp = do
    title <- askLine "Title: "
    description <- fmap strMaybe $ askLine "Description (optional): "
    -- deadline <- askLine "Deadline: "
    deadline <- pure Nothing
    tags <- fmap (readTags . T.unpack) $ askLine "Tags: "
    putTextLn "Priority:"
    urgency <- fmap (readMaybe . T.unpack) $ askLine "  Urgency: " :: IO (Maybe Int)
    importance <- fmap (readMaybe . T.unpack) $ askLine "  Importance: " :: IO (Maybe Int)
    let priority = Priority <$> urgency <*> importance
    let item = fmap (TodoItem title description deadline) tags <*> priority
    list <- todoLoad fp
    let action = do
            list' <- list
            item' <- item
            pure $ do
                putTextLn $ "Add item:\n" <> showt item'
                todoSave fp $ list' <> TodoList [item']
    maybe (putTextLn "Item parsing error") id action
    where
        askLine st = putText st >> hFlush stdout >> getLine
        strMaybe st = T.strip st & \st -> if T.null st then Nothing else Just st

createFunc :: FilePath -> IO ()
createFunc fp = todoSave fp mempty
