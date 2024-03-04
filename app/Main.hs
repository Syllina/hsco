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

data TodoCommand = Save FilePath | Load FilePath | Add FilePath | Create FilePath | List FilePath Bool | Done FilePath Int

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
   <> command "ls" (info listParser ( progDesc "List" ))
   <> command "done" (info doneParser ( progDesc "Done" ))
    ) where
    fileArg = strOption ( long "file" <> short 'f' <> metavar "FILE" <> value "todo.dat" <> action "file")
    saveParser = fmap Save $ argument str (metavar "FILE" <> action "file")
    loadParser = fmap Load fileArg
    addParser = fmap Add fileArg
    createParser = fmap Create fileArg
    listParser = List <$> fileArg <*> (switch ( long "recursive" <> short 'R' <> help "list items recursively"))
    doneParser = Done <$> fileArg <*> (argument auto (metavar "INDEX"))

main :: IO ()
main = do
    cmd <- execParser $ info (mainParser <**> helper)
        ( fullDesc
       <> progDesc "hsco hsco hsco hsco"
       <> header "hsco"
        )
    case cmd of
        Test -> testFunc
        Todo (Save fp) -> todoSave fp defList
        Todo (Load fp) -> todoLoad fp >>= maybe (putTextLn "load error") printT
        Todo (Add fp) -> ((liftA2.liftA2) (addAction fp) (todoLoad fp) getTodoItem) >>= maybe (putTextLn "item parsing error") id
        Todo (Create fp) -> createFunc fp
        Todo (List fp rec) -> ((liftA2.liftA2) (lsAction rec) (todoLoad fp) getTags) >>= maybe (putTextLn "ls error") id
        Todo (Done fp idx) -> ((fmap.fmap) (doneAction fp idx) (todoLoad fp)) >>= maybe (putTextLn "done error") id
        where
            addAction fp list item = do
                putTextLn $ "Add item:\n" <> showt item
                todoSave fp $ list <> TodoList [item]
            getTags = fmap (readTags.T.unpack) $ (putText "Input tags: " >> hFlush stdout >> getLine)
            lsAction False list tags = do
                putTextLn $ "Items containing tags " <> showt tags <> ":"
                printT $ todoWithTags list tags
            lsAction True list tags = do
                putTextLn $ "Items starting with tags " <> showt tags <> ":"
                printT $ todoWithTagsRec list tags
            doneAction fp idx list = maybe
                    (putTextLn $ "Item #" <> showt idx <> " not found.")
                    (\(it, list') -> do
                        putTextLn "Finishing item:"
                        printT it
                        todoSave fp list')
                    (todoDelete list idx)
            

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

getTodoItem :: IO (Maybe TodoItem)
getTodoItem = do
    title <- askLine "Title: "
    description <- fmap strMaybe $ askLine "Description (optional): "
    -- deadline <- askLine "Deadline: "
    deadline <- pure Nothing
    tags <- fmap (readTags . T.unpack) $ askLine "Tags: "
    putTextLn "Priority:"
    urgency <- fmap (readMaybe . T.unpack) $ askLine "  Urgency: " :: IO (Maybe Int)
    importance <- fmap (readMaybe . T.unpack) $ askLine "  Importance: " :: IO (Maybe Int)
    let priority = Priority <$> urgency <*> importance
    pure $ fmap (TodoItem title description deadline) tags <*> priority
    where
        askLine st = putText st >> hFlush stdout >> getLine
        strMaybe st = T.strip st & \st -> if T.null st then Nothing else Just st

-- todoLoad fp :: IO (Maybe TodoList)
-- getTodoItem :: IO (Maybe TodoItem)
-- action :: TodoList -> Item -> IO ()
-- liftA2 :: Applicative f => a -> b -> c -> f a -> f b -> f c
-- liftA2 action :: Maybe TodoList -> Maybe Item -> Maybe IO ()
-- liftA2 (liftA2 action) :: IO (Maybe TodoList) -> IO (Maybe TodoItem) -> IO (Maybe IO ())

createFunc :: FilePath -> IO ()
createFunc fp = todoSave fp mempty
