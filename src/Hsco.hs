{- |
Copyright: (c) 2023 Syllinxia
SPDX-License-Identifier: MIT
Maintainer: Syllinxia <syllinxia@gmail.com>

The very least comprehensive personal haskell tool
-}

module Hsco
       ( someFunc
       , chapterTitle
       ) where

import Prelude hiding ((<|>), many)

import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T

import Data.Char (isPrint)

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)

singleNum :: Parser Int
singleNum = hz2Int <$> oneOf orderedZhNums where
    orderedZhNums = "零一二三四五六七八九"
    hz2Int ch = fromMaybe (-1) $ T.findIndex (ch ==) (T.pack orderedZhNums)

twoDigitNum :: Parser Int
twoDigitNum = compose <$> option 1 singleNum <* char '十' <*> oneDigit where
    compose x y = x * 10 + y
    oneDigit = option 0 singleNum

threeDigitNum :: Parser Int
threeDigitNum = compose <$> singleNum <* char '百' <*> twoDigit where
    compose x y = x * 100 + y
    -- oneDigit = option 0 singleNum
    twoDigit = try twoDigitNum <|> (char '零' *> singleNum) <|> (pure 0)

threeDigitCapNum :: Parser Int
threeDigitCapNum = try threeDigitNum <|> try twoDigitNum <|> try singleNum

chapterTitle :: Parser (Int, Text)
chapterTitle = try normalTitle <|> pure (-1, "sth") where
    normalTitle = compose <$> (normalChapNum <|> (string "尾声" *> pure 0)) <* spaces <*> (T.pack <$> many (satisfy isPrint))
    normalChapNum = (char '第' *> threeDigitCapNum) <* char '章'
    compose 0 b = (0, "尾声 " <> b)
    compose a b = (a, b)
