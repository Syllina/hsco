module Hsco.Thu.Info (
    sayHello
) where

import Hsco.Thu.Internal
import Hsco.Thu.OAuth

import Text.Parsec as P

import Control.Monad.Catch (throwM)

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
