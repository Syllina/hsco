module Hsco.Thu.Learn (
    learnHello
) where

import Hsco.Thu.Internal
import Hsco.Thu.OAuth

import Data.Text (breakOn, strip)
import qualified Data.Text as T
import qualified Data.Text.IO as T

getLoginPostData :: ThuEnv -> [FormParam]
getLoginPostData ThuEnv {..} = ["i_user" := stuID, "i_pass" := stuPwd, "atOnce" := ("true" :: String)]

learnHello :: ThuM ()
learnHello = do
    logInfoN $ "Trying to login into Web Learning"
    r <- redirectThuLoginOnce "/post/bb5df85216504820be7bba2b0ae1535b/0?/login.do"
    -- 此处如果无 WebVPN，可能需要改变 parse 机制
    let url = T.drop (T.length "window.location=\"") $
            fst . breakOn "\";" $ snd . breakOn "window.location=\"" $
            decodeUtf8 (r^.responseBody)
    r <- getThu learnURL (T.unpack url)
    let content = decodeUtf8 $ r^.responseBody
    let Just [name, dept] = scrapeStringLike content parser :: Maybe [Text]
    liftIO $ T.putStrLn $ "你好，来自 " <> dept <> " 的 " <> name <> " !"
    where parser = chroots (("div" @: ["class" @= "fl up-img-info"]) // "p") firstP
          firstP = strip <$> text "label"
