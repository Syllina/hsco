{-# LANGUAGE RecordWildCards, DeriveAnyClass, OverloadedStrings #-}
module Hsco.Thu.OAuth (
    redirectThuLogin,
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

getLoginPostData :: ThuEnv -> [FormParam]
getLoginPostData ThuEnv {..} = ["i_user" := stuID, "i_pass" := stuPwd, "i_captcha" := ("" :: String)]

postThuLogin :: ThuM (Response BS.ByteString)
postThuLogin = asks getLoginPostData >>=
                    postThu idLoginURL "/do/off/ui/auth/login/check"

redirectThuLogin :: ThuM (Response BS.ByteString)
redirectThuLogin = action where
    action = do
        r <- postThuLogin
        let res = parseLoginResponse r
        either handleParse getThuDirect res
    handleParse :: ThuOAuthException -> ThuM (Response BS.ByteString)
    handleParse = throwM
