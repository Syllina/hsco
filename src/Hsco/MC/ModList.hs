{-# LANGUAGE DeriveGeneric #-}
module Hsco.MC.ModList (
    ModList(..),
    ModListItem(..),
    ModSource(..),
    ModDownloadType(..),
    ModAction(..),
    loadModList,
    saveModList
) where

-- import GHC.Generics
import Data.Aeson

data ModSource = Modrinth deriving stock (Generic, Show, Eq, Ord)
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
    mlVersion :: Maybe (Text, Text),
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

data ModAction = ModAction (Maybe ModListItem, Maybe ModListItem) deriving (Show)
