{-# LANGUAGE OverloadedStrings, DeriveAnyClass, RecordWildCards #-}
module Hsco.Thu.WebVPN (
    withWebVPN
) where

import qualified Network.Wreq.Session as S

import Hsco.Thu.Internal
import qualified Data.ByteString.Lazy as BS

import qualified Data.Encoding as E
import Data.Encoding.GB18030 as E
import Data.Binary.Get
-- 芝士什么
import Control.Exception
import Control.Monad.Catch (throwM)
import Data.List (isInfixOf)

-- TODO 这里改成 Login 的 Exception
data ThuWebVPNException = WrongPassword | ExpiredConnection | UnknownError
    deriving (Show, Eq, Exception)

withWebVPN :: IO (String, String) -> ThuM () -> IO ()
-- 这里无法改成 ThuM a -> IO a，不知道如何修改
withWebVPN getInput action = loop where
    loop = do
        (stuID, stuPwd) <- getInput
        session <- S.newSession
        let usesVPN = False
        let thuEnv = ThuEnv {..}
        result <- try $ runStderrLoggingT (runReaderT loginWebVPN thuEnv)
        dealWith (thuEnv { usesVPN = True }) result
        where dealWith :: ThuEnv -> Either ThuWebVPNException () -> IO ()
              dealWith thuEnv (Right _) = runStderrLoggingT (runReaderT action thuEnv)
              dealWith _ (Left WrongPassword) = putStrLn "清华 WebVPN 登录失败：用户名或密码错误" >> loop
              dealWith _ (Left ExpiredConnection) = putStrLn "清华 WebVPN 登录失败：连接超时 / 未知错误" >> loop
              dealWith _ (Left UnknownError) = putStrLn "清华 WebVPN 登录失败：未知错误" >> loop

withWebVPNDefault :: ThuM () -> IO ()
withWebVPNDefault action = withWebVPN getInput action where
    getInput = undefined

loginWebVPN :: ThuM ()
loginWebVPN = local (\env -> env { usesVPN = False } ) $ do
    logInfoN $ "Trying to login into ThuWebVPN"
    _ <- getThu webVPNURL "/login?oauth_login=true"
    logInfoN $ "Attempting to login using OAuth"
    r <- postThuLogin
    validateContentType r (r ^. responseHeader "content-type")
    userID <- asks stuID
    validateSetCookie r userID (r ^? responseHeader "set-cookie")
    let Just url = decodeUtf8 <$> scrapeStringLike (r ^. responseBody) (attr "href" "a")
    _ <- getThuDirect url
    logInfoN $ "Login successfully"
    where
        validateContentType :: (Response BS.ByteString) -> ByteString -> ThuM ()
        validateContentType r str = do
            when (str == "text/html;charset=GBK") $ do
                logErrorN $ "WebVPN: encountered weird response\nContent:\n" <> show r
                let str' = runGet (E.decode E.GB18030) (r ^. responseBody)
                let res = scrapeStringLike str' (text "pre")
                throwM $ errorMessage res
            when (str /= "text/html;charset=UTF-8") $ do
                logErrorN $ "WebVPN: encountered impossible content-type: "
                    <> (decodeUtf8 str) <> "\nContent:\n" <> show r
                throwM UnknownError
            where errorMessage (Just res')
                      | "由于您长时间未操作，浏览器中暂存的信息已经丢失。请您尝试按步骤再操作一次。"
                        `isInfixOf` res' = ExpiredConnection
                  errorMessage _ = UnknownError
        validateSetCookie :: (Response BS.ByteString) -> String -> (Maybe ByteString) -> ThuM ()
        validateSetCookie r _ Nothing = do
            let str = decodeUtf8 (r ^. responseBody)
                res = scrapeStringLike str
                    (text $ "span" @: ["id" @= "msg_note"])
                exception = errorMessage res
            when (exception == WrongPassword) $
                logInfoN "WebVPN: wrong password"
            when (exception /= WrongPassword) $
                logErrorN $ "WebVPN: other failure\nContent:\n" <> show r
            throwM $ exception
            where errorMessage (Just res')
                      | "您的用户名或密码不正确，请重试！" `isInfixOf` res' = WrongPassword
                  errorMessage _  = UnknownError
        validateSetCookie _ userID (Just st)
            | st == "TSINGHUAUSERID=" <> (fromString userID) <>
                "; Expires=Thu, 01-Jan-1970 00:00:10 GMT; Path=/"
                = pure ()
            | otherwise = throwM $ UnknownError
