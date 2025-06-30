module Main (main) where

import Hsco (chapterTitle)
import Control.Lens
import Control.Concurrent (threadDelay)

import Network.Wreq as W hiding (manager)
import Text.HTML.Scalpel hiding (manager)
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Control.Exception (handle)

import qualified Data.Text as T

import TextShow

import System.Directory (doesFileExist)

import Text.Parsec

repeatedGet delay _ _ | delay > 128000000 = pure Nothing
repeatedGet delay opts url = handle (\(HttpExceptionRequest _ _) -> putTextLn ("Retrying " <> T.pack url <> "...") >> threadDelay delay >> repeatedGet (delay*2) opts url) $ Just <$> W.getWith opts url

buildNewFile indexedList = do
    writeFileText "ydhbjc.tex" "\\documentclass{ctexart}\n\\usepackage[margin=1in]{geometry}\n\\fontsize{18pt}{1.2}\n\\begin{document}\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    writeFileText "tmp.in" ""
    forM_ indexedList $ \(idx, (name, _)) -> whenM (doesFileExist $ "chap/ch" <> show idx <> ".tex") $ do
        content <- readFileBS $ "chap/ch" <> show idx <> ".tex"
        let toSection (Right (1, _)) = "\\section{unknown}\n\\subsection*{" <> name <> "}\n"
            toSection _ = "\\subsection*{" <> name <> "}\n"
            -- toSection _ = "what? " <> name <> "\n"
        appendFileText "ydhbjc.tex" $ toSection $ parse chapterTitle "" name
        appendFileBS "ydhbjc.tex" content


    appendFileText "ydhbjc.tex" "\\end{document}"

main :: IO ()
main = do
    -- let url = "http://www.ydhbjc.com/169_169734/64239730.html"
    let root = "http://www.ydhbjc.com"
    let bookID = "/169_169734"
    let opts = defaults

    r <- W.getWith opts $ root <> bookID
    let content = decodeUtf8 $ r^.responseBody :: Text
    writeFileText "toc.html" content
    
    -- putTextLn content
    -- whenJust (scrapeStringLike content (text $ "div" @: ["id" @= "content"])) putTextLn
    let Just chapterList = scrapeStringLike content getTOC where
        getTOC = chroot ("div" @: ["id" @= "list"]) $ chroots "dd" parseSingle
        parseSingle = do
            chapterName <- text "a"
            chapterURL <- attr "href" "a"
            pure (chapterName, chapterURL)

    putTextLn $ "Total chapters: " <> (showt $ length chapterList)
    writeFileText "toc" $ T.unlines $ map (\(idx, (name, url)) -> showt idx <> ": " <> name) $ zip ([1..] :: [Int]) chapterList

    let indexedList = zip ([0..] :: [Int]) chapterList

    buildNewFile indexedList

    let processCond (idx, _) = not <$> (doesFileExist $ "chap/ch" <> show idx <> ".tex")
        processCond _ = pure True
    -- processList <- filterM processCond (zip ([0..] :: [Int]) chapterList) where cond = pure False
    -- why wrong?
    processList <- filterM processCond indexedList
            
    forM_ processList $ \(idx, (name, url)) -> do
        threadDelay 1000000
        res <- repeatedGet 1000000 opts $ root <> T.unpack url
        case res of
            Just r -> do
                let content = decodeUtf8 $ r^.responseBody :: Text

                let Just chapterContent = scrapeStringLike content (text $ "div" @: ["id" @= "content"])

                -- when (idx `mod` 10 == 0) $ threadDelay 5000000
                putTextLn $ "Processing " <> showt idx <> "/1043"

                let result = (T.replace (T.singleton '\r') "\n\n" chapterContent) <> "\n" :: Text
                writeFileText ("chap/ch" <> show idx <> ".tex") result
            Nothing -> pure ()
