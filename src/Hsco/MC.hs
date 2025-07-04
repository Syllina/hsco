module Hsco.MC (
    getProject,
    getProjectVersions,
    downloadVersion,
    Project(..),

    module Hsco.MC.ModList,
    module Hsco.MC.Modrinth
) where

import Hsco.MC.ModList
import Hsco.MC.Modrinth

import Network.Wreq as W
import Control.Lens
import Data.Aeson
import qualified Data.Text as T (unpack)

import TextShow

import qualified Data.ByteString.Lazy as LBS

--------
-- Network

url :: Text
url = "https://api.modrinth.com/v2"

-- assert utf-8 encoding
-- the default ByteString in relude is the strict version?
fetch :: (W.Options -> W.Options) -> Text -> IO LBS.ByteString
fetch opts path = do
    r <- W.getWith (defaults & header "User-Agent" .~ ["Syllina/hsco"] & opts) (T.unpack $ url <> path)
    pure $ (r^.responseBody :: LBS.ByteString)

--------
-- Project

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

-- get project info from project id
getProject :: Text -> IO (Maybe Project)
getProject projectID = decode <$> fetch id ("/project/" <> projectID)

--------
-- Version

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
