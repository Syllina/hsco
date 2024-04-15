module Main (main) where

import Hsco (someFunc)

import Network.Wreq as W
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T (unpack)

import TextShow

import qualified Data.ByteString.Lazy as LBS

data ProjectSearchResult = ProjectSearchResult {
    projResTitle :: Text,
    projResID :: Text
}

instance FromJSON ProjectSearchResult where
    parseJSON = withObject "ProjectSearchResult" $ \v -> ProjectSearchResult
        <$> v .: "title"
        <*> v .: "project_id"

instance TextShow ProjectSearchResult where
    showb (ProjectSearchResult {..}) =
        "Project Title: " <> showb projResTitle

data Project = Project {
    title :: Text,
    projectID :: Text
}
instance FromJSON Project where
    parseJSON = withObject "Project" $ \v -> do
        title <- v .: "title"
        projectID <- v .: "id"
        pure $ Project {..}

instance TextShow Project where
    showb (Project {..}) =
        "Project Title: " <> showb title

url :: Text
url = "https://api.modrinth.com/v2"

-- assert utf-8 encoding
-- the default ByteString in relude is the strict version?
fetch :: (W.Options -> W.Options) -> Text -> IO LBS.ByteString
fetch opts path = do
    r <- W.getWith (defaults & header "User-Agent" .~ ["Syllina/hsco"] & opts) (T.unpack $ url <> path)
    pure $ (r^.responseBody :: LBS.ByteString)

-- get project info from project id
getProject :: Text -> IO (Maybe Project)
getProject projectID = decode <$> fetch id ("/project/" <> projectID)

data VersionFile = VersionFile {
    -- using sha256
    -- fileHash :: Text,
    fileURL :: Text,
    fileName :: Text,
    filePrimary :: Bool
}
instance FromJSON VersionFile where
    parseJSON = withObject "VersionFile" $ \v -> do
        -- fileHash <- v .: "hashes"
        fileURL <- v .: "url"
        fileName <- v .: "filename"
        filePrimary <- v .: "primary"
        pure (VersionFile {..})
downloadFile :: FilePath -> VersionFile -> IO ()
downloadFile dir (VersionFile {..}) = do
    r <- W.getWith defaults $ T.unpack fileURL
    writeFileLBS (dir <> T.unpack fileName) (r^.responseBody)

data Version = Version {
    verName :: Text,
    verNumber :: Text,
    verLoaders :: [Text],
    verGameVersion :: [Text],
    verType :: Text,
    verFiles :: [VersionFile]
}
instance FromJSON Version where
    parseJSON = withObject "Version" $ \v -> do
        -- this may cause runtime error (e.g. no verFiles)
        verName <- v .: "name"
        verNumber <- v .: "version_number"
        verLoaders <- v .: "loaders"
        verGameVersion <- v .: "game_versions"
        verType <- v .: "version_type"
        verFiles <- v .: "files"
        pure $ Version {..}
instance TextShow Version where
    showb (Version {..}) =
        showb verName
        <> fromText ": (loaders: " <> showb verLoaders
        <> fromText ", minecraft_version: " <> showb verGameVersion
        <> fromText ", type: " <> showb verType
        <> fromText ")"
downloadVersion :: FilePath -> Version -> IO ()
downloadVersion dir (Version {..}) = forM_ verFiles $ \file -> when (filePrimary file) $ downloadFile dir file

getProjectVersions :: Text -> IO (Maybe [Version])
getProjectVersions projectID = decode <$> fetch ((param "game_versions" .~ ["[\"1.20.4\"]"]) . (param "loaders" .~ ["[\"fabric\"]"])) ("/project/" <> projectID <> "/version")

modrinth :: IO ()
modrinth = do
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

main :: IO ()
main = do
    let modList = ["fabric-api", "sodium", "iris", "lithium", "modmenu", "entityculling", "ferrite-core", "indium", "sodium-extra", "reeses-sodium-options", "immediatelyfast", "yacl", "memoryleakfix", "lambdynamiclights", "simple-voice-chat", "audioplayer", "sound-physics-remastered", "replay-voice-chat", "replaymod", "server-replay", "enhanced-groups"]
    -- let modList = ["fabric-api", "sodium"]
    -- let curID = "AANobbMI"
    forM_ modList $ \curID -> do
        project <- getProject curID
        whenNothing_ project $ putTextLn $ "Project " <> curID <> " not found!"
        whenJust project $ \project -> do
            putTextLn $ "Downloading " <> title project
            versions <- getProjectVersions curID
            maybe (putTextLn "No versions available!") (\versions -> whenNotNull versions (downloadVersion "mods/" . head)) versions


-- Download from github?

-- Fabric API; fabric-api
-- Sodium; sodium
-- Iris Shaders; iris; not compatible with Distant Horizons
-- Lithium; lithium
-- Mod Menu; modmenu; requires fabric api
-- Entity Culling; entityculling
-- FerriteCore; ferrite-core; not compatible with Hydrogen
-- Indium; indium; requires sodium & fabric api
-- Sodium Extra; sodium-extra; requires sodium
-- Reese's Sodium Options; reeses-sodium-options; requires sodium
-- ImmediatelyFast; immediatelyfast
-- YetAnotherConfigLib; yacl
-- Memory Leak Fix; memoryleakfix
-- LambDynamicLights; lambdynamiclights
-- Simple Voice Chat; simple-voice-chat
-- AudioPlayer; audioplayer
-- Sound Physics Remastered; sound-physics-remastered 
-- Replay Voice Chat; replay-voice-chat
-- ReplayMod; replaymod
-- ServerReplay; server-replay
-- Simple Voice Chat Enhanced Groups; enhanced-groups
