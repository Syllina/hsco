module Hsco.Thu.Info (
    sayHello
) where

import Hsco.Thu.Internal
import Hsco.Thu.OAuth

import Text.Parsec as P

import Control.Monad.Catch (throwM)

getCourseGrade :: String -> ThuM ()
getCourseGrade courseID = do
    r <- getThuDirect $ "https://thos.tsinghua.edu.cn/fp/view?m=fp"
    liftIO $ putStrLn $ decodeUtf8 $ r^.responseBody

sayHello :: ThuM ()
sayHello = do
    _ <- getThu info2021URL "/f/redirectLoginUrl?type=u"
    r <- redirectThuLoginCheck
    let parseResult = scrapeStringLike (decodeUtf8 $ r^.responseBody) (text $ "div" @: ["id" @= "person"]) :: Maybe Text
    nameText <- maybe (throwM ThuException) pure parseResult
    let nameParser = spaces *> manyTill anyChar space
    name <- either (const $ throwM ThuException) pure $
        parse nameParser "" nameText
    liftIO $ putStrLn $ "Hi, " <> name <> "!"
    -- getCourseGrade "10421373"
