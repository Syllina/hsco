module Hsco.Todo (
    Tag(..),
    defItem, defList,
    todoLoad, todoSave
) where

import Data.Time
import TextShow
import TextShow.Data.Time ()
-- import System.IO

newtype Tag = Tag [Text] -- a.b.c ==> [a, b, c] ==> /a/b/c/{title}
    deriving stock (Eq, Show, Read)
instance TextShow Tag where
    -- [a, b, c] ===> /a/b/c
    showb (Tag tag) = mconcat $ (fromText "/" :) $ intersperse (fromText "/") $ fmap fromText tag

isSubTagOf :: Tag -> Tag -> Bool
isSubTagOf (Tag a) (Tag b) = a `isPrefixOf` b

data Priority = Priority {
    urgency :: Int,
    importance :: Int
} deriving stock (Show, Read)
instance TextShow Priority where
    showb (Priority {..}) =
        fromText "urg " <> showb urgency <>
        showbCommaSpace <> "imp " <> showb importance

data TodoItem = TodoItem {
    title :: Text,
    description :: Maybe Text,
    deadline :: Maybe LocalTime,
    tags :: [Tag],
    priority :: Priority
} deriving stock (Show, Read)

defItem :: TodoItem
defItem = TodoItem {
    title = "114514",
    description = Just "1919810",
    deadline = Nothing,
    tags = [Tag ["a", "b"]],
    priority = Priority {
        urgency = 10,
        importance = -10
    }
}
instance TextShow TodoItem where
    showb (TodoItem {..}) =
        fromText title <> endl <>
        fmap (\d -> fromText "描述: " <> fromText d <> endl) description ?<>
        fmap (\d -> fromText "ddl: " <> showb d <> endl) deadline ?<>
        (if null tags then fromText "" else fromText "tag: " <> showb tags <> endl) <>
        showb priority where
            infixr 6 ?<>
            -- infixr 6 <>
            a ?<> b = maybe b (<> b) a
            endl = fromText "\n"

-- data TodoListNode = TagNode Text | ItemNode TodoItem

newtype TodoList = TodoList [TodoItem] deriving stock (Show, Read)
defList :: TodoList
defList = TodoList [defItem]

todoWithTags :: TodoList -> [Tag] -> TodoList
todoWithTags (TodoList list) queryTags = TodoList $ filter hasTag list where
    hasTag (TodoItem {..}) = or [x == y | x <- queryTags, y <- tags]

todoWithTagsRec :: TodoList -> [Tag] -> TodoList
todoWithTagsRec (TodoList list) queryTags = TodoList $ filter hasTag list where
    hasTag (TodoItem {..}) = or [x `isSubTagOf` y | x <- queryTags, y <- tags]

todoSave :: FilePath -> TodoList -> IO ()
-- todoSave fp list = withFile fp WriteMode $ \hdl -> hPrint hdl list
todoSave fp list = writeFile fp $ show list
todoLoad :: FilePath -> IO (Maybe TodoList)
-- todoLoad fp = withFile fp ReadMode $ \hdl -> 
todoLoad fp = readFile fp >>= pure . readMaybe
