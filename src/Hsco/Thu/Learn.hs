module Hsco.Thu.Learn (
    learnHello
) where

import Hsco.Thu.Internal
import Hsco.Thu.OAuth

import Network.Wreq.Types (Postable)

import Data.Aeson
import Data.Aeson.Lens

import Data.Text (breakOn, strip)
import TextShow as T
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BS

import Text.Parsec as P

getLoginPostData :: ThuEnv -> [FormParam]
getLoginPostData ThuEnv {..} = ["i_user" := stuID, "i_pass" := stuPwd, "atOnce" := ("true" :: String)]

getLearn :: String -> ThuM (Response BS.ByteString)
getLearn = getThu learnURL

postLearn :: (Postable a) => String -> a -> ThuM (Response BS.ByteString)
postLearn = postThu learnURL

login :: ThuM (Response BS.ByteString)
login = do
    logInfoN $ "Trying to login into Web Learning..."
    r <- redirectThuLoginOnce "/post/bb5df85216504820be7bba2b0ae1535b/0?/login.do"
    -- 此处如果无 WebVPN，可能需要改变 parse 机制
    let url = T.drop (T.length "window.location=\"") $
            fst . breakOn "\";" $ snd . breakOn "window.location=\"" $
            decodeUtf8 (r^.responseBody)
    getLearn (T.unpack url)

data CourseMetaInfo = CourseMetaInfo {
    name :: Text,
    courseID :: String,
    urlID :: String
} deriving (Show)

instance FromJSON CourseMetaInfo where
-- 如何不按顺序写
    parseJSON = withObject "CourseMetaInfo" $ \v -> CourseMetaInfo
        <$> v .: "kcm" -- 课程名
        <*> v .: "kch" -- 课程号
        <*> v .: "wlkcid" -- 网络课程 id

instance TextShow CourseMetaInfo where
    showb CourseMetaInfo {..} =
        "课程名: " <> fromText name <> ", 课程号: " <> T.fromString courseID

data HwMetaInfo = HwMetaInfo {
    title :: Text
} deriving (Show)

instance FromJSON HwMetaInfo where
    parseJSON = withObject "HwMetaInfo" $ \v -> HwMetaInfo
        <$> v .: "bt" -- 标题

instance TextShow HwMetaInfo where
    showb HwMetaInfo {..} =
        "作业名: " <> fromText title

-- 有一个神秘的 `zjh'，似乎与学生对应
learnHello :: ThuM ()
learnHello = do
    r <- login
    let content = decodeUtf8 $ r^.responseBody
    let csrfParser = string "_csrf=" *> P.many validChar
        validChar = hexDigit P.<|> char '-'
        Right csrf = parse csrfParser "" (snd $ breakOn "_csrf=" content)
    -- let csrf = decodeUtf8 $ r^.responseCookie "XSRF-TOKEN" . cookieValue
    r <- getLearn $ "/b/wlxt/kc/v_wlkc_xs_xkb_kcb_extend/student/loadCourseBySemesterId/2023-2024-1/zh?_csrf=" <> csrf
    let Just coursesList = r^?responseBody . key "resultList"
        Success courses = fromJSON coursesList :: Result [CourseMetaInfo]
    -- r <- getLearn $ "/f/wlxt/index/course/student/course?wlkcid=" <> urlID course
    let proc = \course -> do
            r <- postLearn ("/b/wlxt/kczy/zy/student/zyListWj?_csrf=" <> csrf <> "&_csrf=" <> csrf) ["aoData" := ("[{\"name\":\"wlkcid\",\"value\":\"" <> urlID course <> "\"}]" :: String)]
            let Just hwList = r^?responseBody . key "object" . key "aaData"
                Success hws = fromJSON hwList :: Result [HwMetaInfo]
            liftIO $ printT hws
    mapM_ proc courses
