{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Hsco.Thu (
    ThuEnv,
    initThuEnv,
    loginWebVPN
) where

import Network.Wreq
import qualified Network.Wreq.Session as S
import Network.Wreq.Types (Postable)
import Control.Lens

import qualified Data.ByteString.Lazy as BS

import Control.Monad.Logger
-- 如果外层代码执行在 Logger 下，内层代码加入了 ReaderT 的功能
-- 二者是否可以共用一个 Logger
-- 如果可以的话，这个 Logger 是否需要在外部定义成一个新的 Monad

import qualified Data.Encoding as E
import Data.Encoding.GB18030 as E
import Data.Binary.Get
-- 芝士什么

data ThuEnv = ThuEnv {
    stuID :: String,
    stuPwd :: String,
    session :: S.Session
}

initThuEnv :: String -> String -> IO ThuEnv
initThuEnv stuID stuPwd = do
    session <- S.newSession
    return $ ThuEnv {..}

type ThuM = ReaderT ThuEnv (LoggingT IO)

withSessionIO :: (S.Session -> IO a) -> ThuM a
withSessionIO func = asks session >>= liftIO . func

getThu :: String -> ThuM (Response BS.ByteString)
getThu url = withSessionIO $ \sess -> S.get sess url

postThu :: (Postable a) => String -> a -> ThuM (Response BS.ByteString)
postThu url val = withSessionIO $ \sess -> S.post sess url val

data URLType = URLRaw | URLVPN

getThuURL :: String -> String
getThuURL name = "https://" <> name <> ".tsinghua.edu.cn"

vpnPrefix :: String
vpnPrefix = getThuURL "webvpn" <> "/https"

webVPNURL :: URLType -> String
webVPNURL _ = getThuURL "webvpn"

idLoginURL :: URLType -> String
idLoginURL URLRaw = getThuURL "id"
idLoginURL URLVPN = vpnPrefix <> "/77726476706e69737468656265737421f9f30f8834396657761d88e29d51367bcfe7"

getLoginPostData :: ThuEnv -> [FormParam]
getLoginPostData ThuEnv {..} = ["i_user" := stuID, "i_pass" := stuPwd, "i_captcha" := ("" :: String)]

postThuLogin :: String -> ThuM (Response BS.ByteString)
postThuLogin url = asks getLoginPostData >>= postThu url

loginWebVPN :: ThuM ()
loginWebVPN = do
    logInfoN $ "Trying to login into ThuWebVPN"
    r <- getThu (webVPNURL URLRaw <> "/login?oauth_login=true")
    logInfoN $ "Attempting to login using OAuth"
    r <- postThuLogin (idLoginURL URLRaw <> "/do/off/ui/auth/login/check")
    -- liftIO $ putStr $ runGet (E.decode E.GB18030) (r ^. responseBody)
    liftIO $ putStr $ decodeUtf8 (r ^. responseBody)
    liftIO $ print $ r ^. responseHeader "content-type"
    liftIO $ print $ r ^? responseHeader "set-cookie"
    -- liftIO $ print $ r ^. responseBody
    pure ()
