module Main (main) where

-- import Hsco (someFunc)
-- import Hsco.MC (loadModList, saveModList, genActions, procActions, ModList(..), ModListItem(..), Mod)
import Hsco.MC

import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
-- import System.IO (hFlush, stdout)
import qualified Data.Text as T

import Options.Applicative

data Argument = Argument {
    modsDirectory :: Text,
    modListFile :: Text
}

parser :: Parser Argument
parser = Argument <$> strOption (long "outdir" <> short 'o' <> metavar "DIRECTORY" <> help "the output directory")
                  <*> strOption (long "file" <> short 'f' <> metavar "FILE" <> help "the mod list file")
                  -- <$> strOption (long "version" <> long "ver" <> metavar "VERSION" <> help "minecraft game version (e.g. \"1.21.1\")")
opts :: ParserInfo Argument
opts = info (parser <**> helper) fullDesc

main :: IO ()
main = do
    options <- execParser opts
    let generateModrinthManuals mods = flip map mods $ \identity -> ModListItem {
            mlName = "",
            mlSource = Modrinth,
            mlIdentity = identity,
            mlVersion = Nothing,
            mlType = Manual
        }
    let modList = ModList {
            gameVersion = "1.21.1",
            modItems = generateModrinthManuals ["sodium", "xaeros-minimap"]
        }

    -- saveModList "modlist.txt" modList
    res <- loadModList $ T.unpack (modListFile options)
    let modList = case res of
            Left _ -> ModList { gameVersion = "1.21.1", modItems = [] }
            Right list -> list

    createDirectoryIfMissing True (T.unpack $ modsDirectory options)

    (actions, newList) <- genActions modList

    showActions actions

    putText "Continue? [y/n] " >> hFlush stdout

    inp <- getLine

    when (inp == "y") $ do
        saveModList (T.unpack $ modListFile options) newList
        procActions ((T.unpack $ modsDirectory options) </> "") actions

    pure ()
    -- let modList = ["fabric-api", "sodium", "iris", "lithium", "modmenu", "entityculling", "ferrite-core", "indium", "sodium-extra", "reeses-sodium-options", "immediatelyfast", "yacl", "memoryleakfix", "lambdynamiclights", "simple-voice-chat", "audioplayer", "sound-physics-remastered", "replay-voice-chat", "replaymod", "server-replay", "enhanced-groups"]
    -- -- let modList = ["fabric-api", "sodium"]
    -- -- let curID = "AANobbMI"
    -- forM_ modList $ \curID -> do
    --     project <- getProject curID
    --     whenNothing_ project $ putTextLn $ "Project " <> curID <> " not found!"
    --     whenJust project $ \project -> do
    --         putTextLn $ "Downloading " <> title project
    --         versions <- getProjectVersions curID
    --         maybe (putTextLn "No versions available!") (\versions -> whenNotNull versions (downloadVersion "mods/" . head)) versions


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
