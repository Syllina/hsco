{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hsco.Reco.Arcanist (
    AttributeGenerator,
    fromRaw,
    genAttrMaybe,
    Insight,
    Level
) where

-- this could be a kind!
newtype Insight = Insight Int deriving (Eq, Num)
newtype Level = Level Int

linearInterpolate :: (Int, Int) -> (Int, Int) -> Int -> Double
linearInterpolate (x1', y1') (x2', y2') = \x -> (y2 - y1) / (x2 - x1) * (fromIntegral x - x1) + y2 where
    x1 = fromIntegral x1'
    y1 = fromIntegral y1'
    x2 = fromIntegral x2'
    y2 = fromIntegral y2'

data AttributeGenerator = AttributeGenerator {
    insight :: Int, -- Insight Bonus
    attr01 :: Int, -- attribute at insight 0 level 1
    attr030 :: Int,
    attr140 :: Int,
    attr250 :: Int,
    attr360 :: Int
}

fromRaw :: (Int, Int, Int, Int, Int, Int) -> AttributeGenerator
fromRaw (insight, attr01, attr030, attr140, attr250, attr360) = AttributeGenerator {..}

genAttrMaybe :: (Insight, Level) -> AttributeGenerator -> Maybe Int
genAttrMaybe (0, Level lvl) (AttributeGenerator {..})
    | 1 <= lvl && lvl <= 30 = Just $ floor $ linearInterpolate (1, attr01) (30, attr030) lvl
    | otherwise = Nothing

genAttrMaybe (1, Level lvl) (AttributeGenerator {..})
    | 1 <= lvl && lvl <= 40 = Just $ floor $ linearInterpolate (0, attr030 + insight) (40, attr140) lvl
    | otherwise = Nothing

genAttrMaybe (2, Level lvl) (AttributeGenerator {..})
    | 1 <= lvl && lvl <= 50 = Just $ floor $ linearInterpolate (0, attr140 + insight) (50, attr250) lvl
    | otherwise = Nothing

genAttrMaybe (3, Level lvl) (AttributeGenerator {..})
    | 1 <= lvl && lvl <= 60 = Just $ floor $ linearInterpolate (0, attr250 + insight) (60, attr360) lvl
    | otherwise = Nothing

genAttrMaybe _ _ = Nothing
