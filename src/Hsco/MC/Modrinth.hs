module Hsco.MC.Modrinth () where

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
