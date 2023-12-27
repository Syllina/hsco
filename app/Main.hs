{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main (main) where

import Hsco

import Data.ByteString.Lazy as BS
import Network.Wreq
-- import qualified Network.Wreq as W
import qualified Network.Wreq.Session as S
import Control.Lens
import Control.Monad.Logger

import Text.HTML.Scalpel

import System.Console.Haskeline

data UserData = UserData {
    username :: String,
    password :: String
}

getUserData :: IO UserData
getUserData = runInputT defaultSettings $ do
    Just username <- getInputLine "Username: "
    Just password <- getPassword Nothing "Password: "
    return $ UserData {..}

info2021URL :: String
info2021URL = "https://webvpn.tsinghua.edu.cn/https/77726476706e69737468656265737421f9f9479375603a01301c9aa596522b208e9cd9c9e383ff3f"

idLoginURL :: String
idLoginURL = "https://webvpn.tsinghua.edu.cn/https/77726476706e69737468656265737421f9f30f8834396657761d88e29d51367bcfe7"

main2 :: IO ()
main2 = do
    UserData {..} <- getUserData
    sess <- S.newSession
    -- r <- S.customHistoriedMethodWith "GET" defaults sess "http://webvpn.tsinghua.edu.cn/login?oauth_login=true"
    -- print $ r ^. hrRedirects
    -- print $ r ^. hrFinalRequest
    r <- S.get sess "https://webvpn.tsinghua.edu.cn/login?oauth_login=true"
    r <- S.post sess "https://id.tsinghua.edu.cn/do/off/ui/auth/login/check" ["i_user" := username, "i_pass" := password, "i_captcha" := ("" :: String)]
    print $ r ^. responseBody
    let Just url = decodeUtf8 <$> scrapeStringLike (r ^. responseBody) (attr "href" "a")
    r <- S.get sess url
    r <- S.get sess (info2021URL <> "/f/redirectLoginUrl?type=u")
    r <- S.post sess (idLoginURL <> "/do/off/ui/auth/login/check") ["i_user" := username, "i_pass" := password, "i_captcha" := ("" :: String)]
    let Just url = decodeUtf8 <$> scrapeStringLike (r ^. responseBody) (attr "href" "a")
    r <- S.get sess url
    r <- S.get sess (info2021URL <> "")
    -- BS.putStr $ r ^. responseBody
    return ()

main3 :: IO ()
main3 = do
    UserData {..} <- getUserData
    thuEnv <- initThuEnv username password
    runStderrLoggingT (runReaderT loginWebVPN thuEnv)

main :: IO ()
main = main3
