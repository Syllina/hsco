{-# LANGUAGE NamedFieldPuns, DeriveAnyClass #-}
module Hsco.Thu.Internal (
    ThuEnv(..), initThuEnv,
    ThuM,
    getThu, postThu, 
    getThuDirect, postThuDirect,

    URLType(..),
    getThuURL, vpnPrefix,
    webVPNURL, idLoginURL, info2021URL, learnURL,

    ThuException(..),

    module Control.Monad.Logger,
    module Network.Wreq,
    module Control.Lens,
    module Text.HTML.Scalpel
) where

import Network.Wreq hiding (manager)
import qualified Network.Wreq.Session as S
import Network.Wreq.Types (Postable)
import Text.HTML.Scalpel hiding (manager)
import Control.Lens

import qualified Data.ByteString.Lazy as BS

import Control.Monad.Logger
-- 如果外层代码执行在 Logger 下，内层代码加入了 ReaderT 的功能
-- 二者是否可以共用一个 Logger
-- 如果可以的话，这个 Logger 是否需要在外部定义成一个新的 Monad

data ThuEnv = ThuEnv {
    stuID :: String,
    stuPwd :: String,
    session :: S.Session,
    usesVPN :: Bool
}

initThuEnv :: String -> String -> Bool -> IO ThuEnv
initThuEnv stuID stuPwd usesVPN = do
    session <- S.newSession
    return $ ThuEnv {..}

type ThuM = ReaderT ThuEnv (LoggingT IO)

withSessionIO :: (S.Session -> IO a) -> ThuM a
withSessionIO func = asks session >>= liftIO . func

getThuDirect :: String -> ThuM (Response BS.ByteString)
getThuDirect url = withSessionIO $ \sess -> S.get sess url

getThu :: (URLType -> String) -> String -> ThuM (Response BS.ByteString)
getThu url suf = asks getURLType >>= \t -> getThuDirect (url t <> suf)

postThuDirect :: (Postable a) => String -> a -> ThuM (Response BS.ByteString)
postThuDirect url val = withSessionIO $ \sess -> S.post sess url val

postThu :: (Postable a) => (URLType -> String) -> String -> a -> ThuM (Response BS.ByteString)
postThu url suf val = asks getURLType >>= \t -> postThuDirect (url t <> suf) val

data URLType = URLRaw | URLVPN

getURLType :: ThuEnv -> URLType
getURLType ThuEnv { usesVPN } =
    if usesVPN then URLVPN else URLRaw

getThuURL :: String -> String
getThuURL name = "https://" <> name <> ".tsinghua.edu.cn"

vpnPrefix :: String
vpnPrefix = getThuURL "webvpn" <> "/https"

webVPNURL :: URLType -> String
webVPNURL _ = getThuURL "webvpn"

idLoginURL :: URLType -> String
idLoginURL URLRaw = getThuURL "id"
idLoginURL URLVPN = vpnPrefix <> "/77726476706e69737468656265737421f9f30f8834396657761d88e29d51367bcfe7"

info2021URL :: URLType -> String
info2021URL URLRaw = getThuURL "info2021"
info2021URL URLVPN = vpnPrefix <> "/77726476706e69737468656265737421f9f9479375603a01301c9aa596522b208e9cd9c9e383ff3f"

learnURL :: URLType -> String
learnURL URLRaw = getThuURL "learn"
learnURL URLVPN = vpnPrefix <> "/77726476706e69737468656265737421fcf2408e297e7c4377068ea48d546d30ca8cc97bcc"

data ThuException = ThuException deriving (Show, Exception)
