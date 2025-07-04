{-# LANGUAGE DeriveGeneric #-}
module Hsco.MC.ModList (
    ModList(..),
    ModListItem(..),
    ModSource(..),
    ModDownloadType(..),
    ModAction(..),
    loadModList,
    saveModList,
    showActions,
    procActions
) where

-- import GHC.Generics
import Data.Aeson
import Network.Wreq as W
import qualified Data.Sequence as Q
import qualified Data.Text as T
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens ((^.))

import System.FilePath ((</>))
import System.Directory (removeFile)

data ModSource = Modrinth | Unknown deriving stock (Generic, Show, Eq, Ord)
instance ToJSON ModSource
instance FromJSON ModSource

data ModDownloadType = Manual | Depended deriving stock (Generic, Show, Eq, Ord)
instance ToJSON ModDownloadType
instance FromJSON ModDownloadType

data ModListItem = ModListItem {
    mlName :: Text,
    mlSource :: ModSource,
    -- identity should be source-specific and may not be a Text value?
    -- and also version? whether a mod should be updated should depend
    -- on its source
    mlIdentity :: Text,
    -- verID, url, filepath
    mlVersion :: Maybe (Text, Text, Text),
    mlType :: ModDownloadType
} deriving stock (Generic, Show, Eq, Ord)

instance ToJSON ModListItem
instance FromJSON ModListItem

data ModList = ModList {
    gameVersion :: Text,
    modItems :: [ModListItem]
} deriving stock (Generic, Show)

instance ToJSON ModList
instance FromJSON ModList

loadModList :: FilePath -> IO (Either String ModList)
loadModList = eitherDecodeFileStrict

saveModList :: FilePath -> ModList -> IO ()
saveModList = encodeFile

type ModAction = (Maybe ModListItem, Maybe ModListItem)

-- 是否应该传个 IO Action 回来？
downloadFile :: FilePath -> Text -> IO ()
downloadFile filePath url = do
    content <- W.get $ T.unpack url
    writeFileLBS filePath $ content^.responseBody

showActions :: Q.Seq ModAction -> IO ()
showActions actions = forM_ actions $ \action -> case action of
    (Nothing, Just new) -> putTextLn $ "Install " <> (getFile new)
    (Just old, Nothing) -> putTextLn $ "Remove " <> (getFile old)
    (Just old, Just new) -> case mlVersion old of
        Just _ -> putTextLn $ "Update " <> (getFile old) <> " to " <> (getFile new)
        Nothing -> putTextLn $ "Install " <> (getFile new)
    _ -> pure ()
  where
    getFile item = case mlVersion item of
        Just (_, _, file) -> file
        Nothing -> ""

procActions :: FilePath -> Q.Seq ModAction -> IO ()
procActions folder actions = do
    let procAction (Nothing, Just new) = downloadMod new
        procAction (Just old, Nothing) = deleteMod old
        procAction (Just old, Just new) = deleteMod old >> downloadMod new
        procAction _ = pure ()
        downloadMod item = case mlVersion item of
            Just (_, url, filePath) -> downloadFile (folder </> T.unpack filePath) url
            Nothing -> pure ()

        deleteMod item = case mlVersion item of
            Just (_, url, filePath) -> removeFile $ folder </> T.unpack filePath
            Nothing -> pure ()

    mapConcurrently procAction actions

    pure ()
