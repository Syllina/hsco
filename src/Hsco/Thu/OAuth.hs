{-# LANGUAGE DeriveAnyClass #-}
module Hsco.Thu.OAuth (
    redirectThuLoginCheck,
    redirectThuLoginOnce,
    ThuOAuthException(..)
) where

import Hsco.Thu.Internal

import Data.Text (strip, unpack)
import qualified Data.ByteString.Lazy as BS

import Control.Monad.Except
import Control.Monad.Catch (throwM)

data ThuOAuthException = WrongPassword | UnknownError
    deriving (Show, Exception, Eq)
parseLoginResponse :: Response BS.ByteString -> Either ThuOAuthException String
parseLoginResponse r = runExcept $ do
    let contentType = r^.responseHeader "content-type"
    when (contentType /= "text/html;charset=UTF-8") $ throwError UnknownError
    let content = decodeUtf8 $ r^.responseBody
    let errorMsg = strip <$> scrapeStringLike content (text $ "span" @: ["id" @= "msg_note"])
    when (errorMsg == Just "您的用户名或密码不正确，请重试！") $
        throwError WrongPassword
    let urlRes = scrapeStringLike content (attr "href" "a")
    maybe (throwError UnknownError) (return . unpack) urlRes

redirectThuLogin :: String -> (ThuEnv -> [FormParam]) -> ThuM (Response BS.ByteString)
redirectThuLogin url method = action where
    action = asks method >>=
             postThu idLoginURL ("/do/off/ui/auth/login" <> url) >>=
             pure . parseLoginResponse >>=
             either handleParse getThuDirect
    handleParse :: ThuOAuthException -> ThuM (Response BS.ByteString)
    handleParse = throwM

redirectThuLoginCheck :: ThuM (Response BS.ByteString)
redirectThuLoginCheck = redirectThuLogin "/check" getData where
    getData :: ThuEnv -> [FormParam]
    getData ThuEnv {..} = [
            "i_user" := stuID,
            "i_pass" := stuPwd,
            "i_captcha" := ("" :: String)
        ]

redirectThuLoginOnce :: String -> ThuM (Response BS.ByteString)
redirectThuLoginOnce url = redirectThuLogin url getData where
    getData :: ThuEnv -> [FormParam]
    getData ThuEnv {..} = [
            "i_user" := stuID,
            "i_pass" := stuPwd,
            "atOnce" := ("true" :: String)
        ]
