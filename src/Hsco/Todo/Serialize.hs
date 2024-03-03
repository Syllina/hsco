{-# LANGUAGE StandaloneDeriving #-}
module Hsco.Todo.Serialize where

import Prelude hiding (put, get)

import Data.Time
import Data.Serialize
import Hsco.Todo

-- TODO use new types
instance Serialize Text where
    put = (put :: Putter ByteString) . encodeUtf8
    get = fmap decodeUtf8 (get :: Get ByteString)
instance Serialize UTCTime where
    put = (put :: Putter String) . show
    get = get >>= (maybe (fail "parse error") pure . readMaybe)

instance Serialize Tag
instance Serialize Priority
instance Serialize TodoItem
instance Serialize TodoList

todoSave :: FilePath -> TodoList -> IO ()
-- todoSave fp list = withFile fp WriteMode $ \hdl -> hPrint hdl list
-- todoSave fp list = writeFile fp $ show list
todoSave fp list = writeFileBS fp $ encode list
todoLoad :: FilePath -> IO (Maybe TodoList)
-- todoLoad fp = withFile fp ReadMode $ \hdl -> 
-- todoLoad fp = readFileBS fp >>= pure . readMaybe
todoLoad = fmap (either (const Nothing) Just . decode) . readFileBS
