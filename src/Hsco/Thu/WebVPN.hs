{-# LANGUAGE DeriveAnyClass, ScopedTypeVariables #-}
module Hsco.Thu.WebVPN (
    withWebVPN
) where

import Hsco.Thu.Internal
import Hsco.Thu.OAuth

import qualified Network.Wreq.Session as S

import Control.Exception

withWebVPN :: forall a. IO (Text, Text) -> ThuM a -> IO a
-- 这里无法改成 ThuM a -> IO a，不知道如何修改
-- 实际上不加扩展不写局部类型签名是可以过编译的，不知道上回为啥不行
-- 如果写局部类型签名的话就需要加扩展再加一个 forall a.
withWebVPN getInput action = loop where
    loop = do
        (stuID, stuPwd) <- getInput
        session <- S.newSession
        let usesVPN = False
        let thuEnv = ThuEnv {..}
        result <- try $ runStderrLoggingT (runReaderT loginWebVPN thuEnv)
        dealWith (thuEnv { usesVPN = True }) result
        where dealWith :: ThuEnv -> Either ThuOAuthException () -> IO a
              dealWith thuEnv (Right _) = runStderrLoggingT (runReaderT action thuEnv)
              dealWith _ (Left WrongPassword) = putStrLn "清华 WebVPN 登录失败：用户名或密码错误" >> loop
              -- dealWith _ (Left ExpiredConnection) = putStrLn "清华 WebVPN 登录失败：连接超时 / 未知错误" >> loop
              dealWith _ (Left UnknownError) = putStrLn "清华 WebVPN 登录失败：未知错误" >> loop

loginWebVPN :: ThuM ()
loginWebVPN = local (\env -> env { usesVPN = False } ) $ do
    logInfoN $ "Trying to login into ThuWebVPN"
    _ <- getThu webVPNURL "/login?oauth_login=true"
    logInfoN $ "Attempting to login using OAuth"
    _ <- redirectThuLoginCheck
    pure ()
