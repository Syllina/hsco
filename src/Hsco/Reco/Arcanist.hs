{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, KindSignatures, GADTs, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, TypeFamilies #-}
module Hsco.Reco.Arcanist (
    StatGenerator,
    fromRaw,
    genStatMaybe,
    Insight(..),
    Level(..),
    ArcanistPlainData(..),
    IsArcanist(..),
    Arcanist(..),
    SomeArcanist(..),
    ArcanistInst(..)
) where

import Hsco.Reco.Resonance

data ArcanistPlainData = ArcanistPlainData {
    atkGen, hpGen, rdefGen, mdefGen, critGen :: StatGenerator
}

class IsArcanist arc where
    arcName :: Text
    arcPlainStat :: ArcanistPlainData
    type ArcResType arc :: ResonanceType

class ArcanistInst arc where
    getName :: arc -> Text

data (IsArcanist arc) => Arcanist arc = Arcanist

instance forall arc. IsArcanist arc => ArcanistInst (Arcanist arc) where
    getName _ = arcName @arc

data SomeArcanist where
    SomeArcanist :: (IsArcanist a) => Arcanist a -> SomeArcanist
instance ArcanistInst SomeArcanist where
    getName (SomeArcanist arc) = getName arc

-- this could be a kind!
newtype Insight = Insight Int deriving newtype (Eq, Num)
newtype Level = Level Int

linearInterpolate :: (Int, Int) -> (Int, Int) -> Int -> Double
linearInterpolate (x1', y1') (x2', y2') = \x -> (y2 - y1) / (x2 - x1) * (fromIntegral x - x1) + y1 where
    x1 = fromIntegral x1'
    y1 = fromIntegral y1'
    x2 = fromIntegral x2'
    y2 = fromIntegral y2'

data StatGenerator = StatGenerator {
    insight :: Int, -- insight bonus
    stat01 :: Int, -- statibute at insight 0 level 1
    stat030 :: Int,
    stat140 :: Int,
    stat250 :: Int,
    stat360 :: Int
}

fromRaw :: (Int, Int, Int, Int, Int, Int) -> StatGenerator
fromRaw (insight, stat01, stat030, stat140, stat250, stat360) = StatGenerator {..}

genStatMaybe :: (Insight, Level) -> StatGenerator -> Maybe Int
genStatMaybe (0, Level lvl) (StatGenerator {..})
    | 1 <= lvl && lvl <= 30 = Just $ floor $ linearInterpolate (1, stat01) (30, stat030) lvl
    | otherwise = Nothing

genStatMaybe (1, Level lvl) (StatGenerator {..})
    | 1 <= lvl && lvl <= 40 = Just $ floor $ linearInterpolate (0, stat030 + insight) (40, stat140) lvl
    | otherwise = Nothing

genStatMaybe (2, Level lvl) (StatGenerator {..})
    | 1 <= lvl && lvl <= 50 = Just $ floor $ linearInterpolate (0, stat140 + insight) (50, stat250) lvl
    | otherwise = Nothing

genStatMaybe (3, Level lvl) (StatGenerator {..})
    | 1 <= lvl && lvl <= 60 = Just $ floor $ linearInterpolate (0, stat250 + insight) (60, stat360) lvl
    | otherwise = Nothing

genStatMaybe _ _ = Nothing
