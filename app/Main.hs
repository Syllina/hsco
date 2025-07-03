module Main (main) where

-- import Hsco (someFunc)
import Hsco.MC (getProject, getProjectVersions, downloadVersion, title)
import Hsco

main :: IO ()
main = do
    someFunc
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
