module Hsco.Thu.Learn (
    learnHello
) where

import Hsco.Thu.Internal
import Hsco.Thu.OAuth

import Network.Wreq.Types (Postable)

import Data.Aeson
import Data.Aeson.Lens

import Data.Text (breakOn)
import TextShow as T
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BS

import Text.Parsec as P

import Control.Monad.Catch (throwM)

login :: ThuM (Response BS.ByteString)
login = do
    logInfoN $ "Trying to login into Web Learning..."
    r <- redirectThuLoginOnce "/post/bb5df85216504820be7bba2b0ae1535b/0?/login.do"
    -- 此处如果无 WebVPN，可能需要改变 parse 机制
    let url = T.drop (T.length "window.location=\"") $
            fst . breakOn "\";" $ snd . breakOn "window.location=\"" $
            decodeUtf8 (r^.responseBody)
    getThu learnURL url

-- getCSRF
-- 每次请求前最好都检查一遍 csrf，或许可以写成 monad 然后 withWebLearning 形式？
-- 或者（怎么优美地保证）在每个导出的函数前都加入 checkLogin
checkLogin :: ThuM Text
checkLogin = do
    -- TODO 这里未判断是否已经登录，因为似乎直接选择重新发送登录请求也会得到一样的 csrf
    r <- login
    let content = decodeUtf8 $ r^.responseBody
    -- 从 body 中的 url 里获取 csrf
    -- let csrf = decodeUtf8 $ r^.responseCookie "XSRF-TOKEN" . cookieValue
    -- 原本打算写成这样的形式，但似乎无效
    let csrfParser = string "_csrf=" *> P.many validChar
        validChar = hexDigit P.<|> char '-'
        parseResult' = T.pack <$> parse csrfParser "" (snd $ breakOn "_csrf=" content)
    either handler pure parseResult'
    -- parse 失败则抛出默认异常
    where handler _ = logErrorN "Web Learning: csrf parse error" >> throwM ThuException

-- with csrf
type ThuLearnM = ReaderT Text ThuM

-- 这里我进行一个所有的 get 都只需要传 csrf 一个参数的大胆假设
-- （否则就需要在 Internal 里导出一个 getThuWith）
askCSRF :: ThuLearnM Text
askCSRF = ask

getLearn :: Text -> ThuLearnM (Response BS.ByteString)
getLearn url = do
    csrfSuffix <- ("_csrf="<>) <$> askCSRF
    lift $ getThu learnURL (url <> "?" <> csrfSuffix)

postLearn :: (Postable a) => Text -> a -> ThuLearnM (Response BS.ByteString)
postLearn url form = do
    csrfSuffix <- ("_csrf="<>) <$> askCSRF
    lift $ postThu learnURL (url <> "?" <> csrfSuffix) form

data CourseMetaInfo = CourseMetaInfo {
    courseName :: Text,
    courseID :: Text,
    urlID :: Text
} deriving stock (Show)

instance FromJSON CourseMetaInfo where
-- 如何不按顺序写
    parseJSON = withObject "CourseMetaInfo" $ \v -> CourseMetaInfo
        <$> v .: "kcm" -- 课程名
        <*> v .: "kch" -- 课程号
        <*> v .: "wlkcid" -- 网络课程 id

instance TextShow CourseMetaInfo where
    showb CourseMetaInfo {..} =
        "课程名: " <> fromText courseName <> ", 课程号: " <> fromText courseID

data HwMetaInfo = HwMetaInfo {
    hwTitle :: Text
} deriving stock (Show)

instance FromJSON HwMetaInfo where
    parseJSON = withObject "HwMetaInfo" $ \v -> HwMetaInfo
        <$> v .: "bt" -- 标题

instance TextShow HwMetaInfo where
    showb HwMetaInfo {..} =
        "作业名: " <> fromText hwTitle

data SemesterMetaInfo = SemesterMetaInfo {
    semID :: Text,
    semName :: Text
} deriving stock (Show)

instance FromJSON SemesterMetaInfo where
    parseJSON = withObject "SemesterMetaInfo" $ \v -> SemesterMetaInfo
        <$> v .: "xnxq" -- 学年学期？
        <*> v .: "xnxqmc" -- 学年学期名称?

instance TextShow SemesterMetaInfo where
    showb SemesterMetaInfo {..} =
        "学期名: " <> fromText semName

parseResult :: Result a -> ThuLearnM a
parseResult (Data.Aeson.Error st) = do
    logErrorN ("WebLearning: JSON parse error: " <> Prelude.fromString st)
    throwM ThuException
parseResult (Data.Aeson.Success res) = pure res

parseMaybe :: Maybe a -> ThuLearnM a
parseMaybe Nothing = logErrorN "WebLearning: Nothing" >> throwM ThuException
parseMaybe (Just a) = pure a

-- TODO 真的需要每次拉学期吗？
-- TODO parse 失败自己报异常的 parser
getCurrentSemester :: ThuLearnM SemesterMetaInfo
getCurrentSemester = do
    r <- getLearn "/b/kc/zhjw_v_code_xnxq/getCurrentAndNextSemester"
    -- result 是当前学期，resultList 只有一个元素，是后一个学期
    semCurrent <- parseMaybe $ r^?responseBody . key "result"
    semester <- parseResult $ (fromJSON semCurrent :: Result SemesterMetaInfo)
    logInfoN $ "WebLearning: 获取到当前学期 " <> (semName semester)
    pure semester

getCourses :: SemesterMetaInfo -> ThuLearnM [CourseMetaInfo]
getCourses SemesterMetaInfo {..} = do
    r <- getLearn $ "/b/wlxt/kc/v_wlkc_xs_xkb_kcb_extend/student/loadCourseBySemesterId/" <> semID <> "/zh"
    coursesList <- parseMaybe $ r^?responseBody . key "resultList"
    parseResult $ (fromJSON coursesList :: Result [CourseMetaInfo])

getHws :: CourseMetaInfo -> ThuLearnM [HwMetaInfo]
getHws course = do
    r <- postLearn "/b/wlxt/kczy/zy/student/zyListWj" ["aoData" := ("[{\"name\":\"wlkcid\",\"value\":\"" <> urlID course <> "\"}]" :: Text)]
    hwsList <- parseMaybe $ r^?responseBody . key "object" . key "aaData"
    parseResult $ (fromJSON hwsList :: Result [HwMetaInfo])

-- 有一个神秘的 `zjh'，似乎与学生对应
learnHello :: ThuM ()
learnHello = do
    csrf <- checkLogin
    -- usingReaderT = flip runReaderT -- from relude
    usingReaderT csrf $ do
        semester <- getCurrentSemester
        courses <- getCourses semester
        -- r <- getLearn $ "/f/wlxt/index/course/student/course?wlkcid=" <> urlID course
        forM_ courses $ \course -> getHws course >>= (liftIO . printT)
        pure ()
