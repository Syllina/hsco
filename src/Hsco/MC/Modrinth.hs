module Hsco.MC.Modrinth (testModrinth) where

-- TODO modrinth 提供了根据 hash 下载文件的接口

import Hsco.MC.ModList

import Network.Wreq as W
-- import Relude.Extra.Lens
import Control.Lens

import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import qualified Data.Sequence as Q

import qualified Data.Text as T

-- import qualified Data.Vector as V
import Data.Aeson (decode, Value(..))
import Data.Aeson.Lens

urlRoot :: Text
urlRoot = "https://api.modrinth.com/v2"

userAgent :: ByteString
userAgent = "Syllina/hsco"

reqOptions :: Options
reqOptions = defaults & header "User-Agent" .~ [userAgent]

-- only version
newtype Env = Env Text

type Modrinth = ReaderT Env IO

-- Step 1. 只保留所有手动安装的模组（的名字）
-- Step 2. 迭代找到所有的依赖
-- Step 3. 找到所有模组的最新版本
-- Step 4. 与当前的 List 比较，生成相应的操作

-- 全是 Lens 可以简化的操作！
getManualMods :: ModList -> S.HashSet Text
getManualMods = S.fromList . map mlIdentity . filter isManual . modItems where
    isManual item = mlType item == Manual

getDependencies :: S.HashSet Text -> Modrinth (S.HashSet Text)
getDependencies manual = proc initQueue S.empty where
    proc Q.Empty deps = pure deps
    proc (mod Q.:<| rem) deps = do
        singleDeps <- getDependency mod
        let (rem', deps') = merge rem deps singleDeps
        proc rem' deps'

    merge queue deps Q.Empty = (queue, deps)
    merge queue deps (mod Q.:<| rem)
        | S.member mod deps || S.member mod manual = (queue, deps)
        | otherwise = (queue Q.:|> mod, S.insert mod deps)

    initQueue = (Q.fromList . S.toList) manual

getDependency :: Text -> Modrinth (Q.Seq Text)
getDependency name = do
    Env gameVersion <- ask
    let opt = reqOptions & param "loaders" .~ ["[\"fabric\"]"] & param "game_versions" .~ ["[\"" <> gameVersion <> "\"]"]
    result <- liftIO $ W.getWith opt $ T.unpack $ urlRoot <> "/project/" <> name <> "/version"

    -- how to enter [{"deps": [{"prj": _}]}]
    let deps = result^.responseBody ^.. nth 0 . key "dependencies" . values . key "project_id" . _String

    pure $ Q.fromList deps

-- type ItemWithURL = (ModListItem, Text)
type ItemWithURL = ModListItem

getModList :: S.HashSet Text -> Modrinth (M.HashMap Text ItemWithURL)
getModList = M.traverseWithKey func . S.toMap where
    func name _ = getListItem name

getListItem :: Text -> Modrinth ItemWithURL
getListItem name = do
    result <- liftIO $ W.getWith reqOptions $ T.unpack $ urlRoot <> "/project/" <> name
    let body = result^.responseBody
        fullName = fromMaybe "unknown" $ body ^? key "title" . _String
        -- TODO maybe monad?
        slug = body ^? key "slug" . _String
        modID = fromMaybe "unknown" $ body ^? key "id" . _String
        identity = fromMaybe modID slug

    Env gameVersion <- ask
    let opt = reqOptions & param "loaders" .~ ["[\"fabric\"]"] & param "game_versions" .~ ["[\"" <> gameVersion <> "\"]"]
    result <- liftIO $ W.getWith opt $ T.unpack $ urlRoot <> "/project/" <> name <> "/version"

    let verID = fromMaybe "unknown" $ result^.responseBody ^? nth 0 . key "id" . _String
        url = fromMaybe "unknown" $ result^.responseBody ^? nth 0 . key "files" . nth 0 . key "url" . _String
        file = fromMaybe "unknown" $ result^.responseBody ^? nth 0 . key "files" . nth 0 . key "filename" . _String

    pure ModListItem {
            mlName = fullName,
            mlSource = Hsco.MC.ModList.Modrinth,
            mlIdentity = identity,
            mlVersion = Just (verID, url),
            -- TODO assert all dep
            mlType = Depended
        }

modListToMap :: ModList -> M.HashMap Text ModListItem
modListToMap = M.fromList . map (\item -> (mlIdentity item, item)) . modItems

genActions :: ModList -> IO (Q.Seq ModAction)
genActions modList = flip runReaderT (Env (gameVersion modList)) $ do
    let manual = getManualMods modList
    deps <- getDependencies manual

    versions <- getModList $ S.union manual deps

    let oldMods = modListToMap modList

    -- 处理已有模组
    let ret1 = M.foldl' step1 Q.empty oldMods
        step1 queue mod = case M.lookup (mlIdentity mod) versions of
            Just item | mlVersion mod == mlVersion item -> queue
                      | otherwise -> queue Q.:|> ModAction (Just mod, Just item { mlType = mlType mod })
            Nothing -> queue Q.:|> ModAction (Just mod, Nothing)

    -- 处理新的模组
        ret2 = M.foldl' step2 ret1 versions
        step2 queue item = case M.lookup (mlIdentity item) oldMods of
            Just mod -> queue
            Nothing -> queue Q.:|> ModAction (Nothing, Just item)

    pure ret2

testModrinth :: IO ()
testModrinth = do
    let opt = reqOptions & param "loaders" .~ ["[\"fabric\"]"] & param "game_versions" .~ ["[\"1.21.1\"]"]
    -- result <- liftIO $ W.getWith reqOptions $ T.unpack $ urlRoot <> "/project/" <> name <> "/version"
    -- BKURGnp1
    -- putLBSLn $ result^.responseBody
    result <- genActions ModList {
        gameVersion = "1.21.1",
        modItems = [ ModListItem {
            mlName = "",
            mlSource = Hsco.MC.ModList.Modrinth,
            mlIdentity = "sodium",
            mlVersion = Nothing,
            mlType = Manual
        }, ModListItem {
            mlName = "",
            mlSource = Hsco.MC.ModList.Modrinth,
            mlIdentity = "xaeros-minimap",
            mlVersion = Nothing,
            mlType = Manual
        } ]
    }
    -- result <- runReaderT (getDependencies (S.fromList ["sodium", "xaeros-minimap"])) (Env "1.21.1")
    print result
    pure ()

-- 理论上不应该在模块里面写这种函数
-- （甚至有可能依赖别的平台？）
-- 但由于实际上在可见的将来我也不打算支持别的平台
getModListDependencies :: ModList -> ([ModListItem], ModList)
getModListDependencies modList = ([], modList) where
    currentMods = M.fromList $ map (\m -> (mlIdentity m, m)) $ modItems modList

-- searchProjects :: 

--------
-- API test

-- modrinth :: IO ()
-- modrinth = undefined
{- modrinth = do
    let opts = defaults & header "User-Agent" .~ ["Syllina/hsco"]
    let url = "https://api.modrinth.com/v2"

-- GET /search
-- https://docs.modrinth.com/#tag/projects/operation/searchProjectSearchResults
    r <- W.getWith (opts & param "query" .~ ["Sodium"] & param "facets" .~ ["[[\"categories:fabric\"], [\"versions:1.20.4\"], [\"project_type:mod\"], [\"license: LGPL-3.0-only\"]]"] & param "limit" .~ ["1"]) $ url <> "/search"
    let content = decodeUtf8 $ r^.responseBody :: Text
    -- putTextLn content
    let Just (Success projects) = fromJSON <$> content ^? key "hits" :: Maybe (Result [ProjectSearchResult])
    let project : _ = projects
    -- let curID = T.unpack $ projectID project
    let curID = "AANobbMI"

-- GET /project/{id|slug}
-- https://docs.modrinth.com/#tag/projects/operation/getProject
    r <- W.getWith opts $ url <> "/project/" <> curID
    let content = decodeUtf8 $ r^.responseBody :: Text
    maybe (pure ()) putTextLn $ content ^? key "title" . _String

-- GET /project/{id|slug}/dependencies
-- https://docs.modrinth.com/#tag/projects/operation/getDependencies
    r <- W.getWith opts $ url <> "/project/" <> curID <> "/dependencies"
    let content = decodeUtf8 $ r^.responseBody :: Text
    -- putTextLn content
    maybe (pure ()) putTextLn $ content ^? key "projects" . nth 0 . key "title" . _String

-- GET /project/{id|slug}/version
-- https://docs.modrinth.com/#tag/versions/operation/getProjectVersions
    r <- W.getWith (opts & param "loaders" .~ ["fabric"]) $ url <> "/project/" <> curID <> "/version"
    let content = decodeUtf8 $ r^.responseBody :: Text
    maybe (pure ()) putTextLn $ content ^? nth 0 . key "id" . _String
    let Just versionID = T.unpack <$> content ^? nth 0 . key "id" . _String

-- GET /version/{id}
-- https://docs.modrinth.com/#tag/versions/operation/getVersion
    r <- W.getWith opts $ url <> "/version/" <> versionID
    let content = decodeUtf8 $ r^.responseBody :: Text
    -- maybe (pure()) putTextLn $ content
    maybe (pure ()) putTextLn $ content ^? key "files" . nth 0 . key "url" . _String
    let Just downloadURL = T.unpack <$> content ^? key "files" . nth 0 . key "url" . _String
    let Just fileName = T.unpack <$> content ^? key "files" . nth 0 . key "filename" . _String

    r <- W.getWith opts $ downloadURL
    writeFileLBS fileName (r^.responseBody)
-}
