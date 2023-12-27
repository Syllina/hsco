{-# LANGUAGE OverloadedStrings #-}
module Hsco.Thu.Info (
    sayHello
) where

import Hsco.Thu.Internal
import Text.Parsec as P

sayHello :: ThuM ()
sayHello = do
    _ <- getThu info2021URL "/f/redirectLoginUrl?type=u"
    r <- redirectThuLogin
    let Just nameText = scrapeStringLike (decodeUtf8 $ r^.responseBody) (text $ "div" @: ["id" @= "person"]) :: Maybe Text
    let Right name = parse p "" nameText where
            p = spaces *> manyTill anyChar space
    liftIO $ putStrLn $ "Hi, " <> name <> "!"
