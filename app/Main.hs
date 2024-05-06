module Main (main) where

import Hsco

import System.Console.Haskeline
import Data.Text as T
import Data.Text.IO as T

import Test.WebDriver
import Test.WebDriver.Commands.Wait

import Data.Aeson

import Network.HTTP.Client.TLS
import Network.HTTP.ReverseProxy
import Network.Wai
import Network.Wai.Handler.Warp (run)

import Control.Concurrent (forkIO)

import Network.WebSockets

-- import Network.Wreq as W
-- import qualified Network.Wreq.Session as S
-- import Control.Lens

main5 :: IO ()
main5 = withWebVPN getInput (sayHello >> learnHello) where
    getInput = runInputT defaultSettings $ do
        (Just username) <- getInputLine "请输入学号: "
        (Just password) <- getPassword Nothing "请输入密码: "
        pure (T.pack username, T.pack password)

main6 :: IO ()
-- main6 = runSession (defaultConfig & useBrowser chrome & useProxy (Manual "127.0.0.1:3000" "127.0.0.1:3000" "127.0.0.1:3000")) $ do
main6 = runSession (defaultConfig & useBrowser chrome) $ do
    openPage "https://pro.yuketang.cn"
    putTextLn "Please login in 20s..."
    waitUntil 20 $ do
        title <- getTitle
        expect $ title == "雨课堂"
    cookies >>= print
    putTextLn ""
    -- openPage "https://pro.yuketang.cn/lesson/fullscreen/v3/1135885338563518208"
    openPage "https://pro.yuketang.cn/api/v3/lesson/presentation/fetch?presentation_id=1135885451650342656"
    getSource >>= putTextLn

    closeSession

main7 :: IO ()
main7 = bingExample >>= run 3000 where
    bingExample = do
        manager <- newTlsManager
        pure $
            waiProxyToSettings
                ( \request -> do
                    print request
                    print $ pathInfo request
                    let rawDomain : _ = pathInfo request
                    print rawDomain
                    let (domain, port) = breakOn ":" rawDomain
                    print (domain, port)
                    pure $ WPRProxyDest $ ProxyDest "127.0.0.1" 7890
                    -- return $
                    --     WPRModifiedRequestSecure
                    --         ( request
                    --             { requestHeaders = [("Host", encodeUtf8 domain)]
                    --             }
                    --         )
                    --         (ProxyDest (encodeUtf8 domain) 443)
                )
                defaultWaiProxySettings {wpsLogRequest = print}
                manager

main8 :: IO ()
main8 = runClient "127.0.0.1" 4444 "/session/114514" $ \connection -> do
    putTextLn "connected!"
    void . forkIO . forever $ do
        message <- receiveData connection
        putTextLn message

main :: IO ()
main = do
    main8
