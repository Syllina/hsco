{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, KindSignatures, GADTs, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
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
    ArcanistInst(..),
    module Hsco.Reco.Resonance,
    module Hsco.Reco.Stat
) where

import Hsco.Reco.Resonance hiding (getStatMod)
import qualified Hsco.Reco.Resonance as Resonance
import Hsco.Reco.Stat

data ArcanistPlainData = ArcanistPlainData {
    atkGen, hpGen, rdefGen, mdefGen, critGen :: StatGenerator
}

class IsArcanist arc where
    arcName :: Text
    arcPlainStat :: ArcanistPlainData
    type ArcResType arc :: ResonanceType

class ArcanistInst arc where
    getName :: arc -> Text
    getPlainStat :: arc -> Maybe Stat
    -- 如果等级可以从类型上验证这里就不需要 Maybe 了
    getResonanceStatMod :: arc -> Maybe StatMod

-- this could be a kind!
newtype Insight = Insight Int deriving newtype (Eq, Num)
newtype Level = Level Int

data (IsArcanist arc) => Arcanist arc = Arcanist {
    arcInsight :: Insight,
    arcLevel :: Level,
    arcResonance :: Resonance (ArcResType arc)
}

instance forall arc. (IsArcanist arc, IsResonanceType (ArcResType arc)) => ArcanistInst (Arcanist arc) where
    getName _ = arcName @arc
    getPlainStat (Arcanist {..}) = do
        let usingGen gen = genStatMaybe (arcInsight, arcLevel) (gen $ arcPlainStat @arc)
        atk <- usingGen atkGen
        hp <- usingGen hpGen
        realDef <- usingGen rdefGen
        mentDef <- usingGen mdefGen
        critTech <- usingGen critGen
        pure $ statDef {
            atk = atk,
            hp = hp,
            realDef = realDef,
            mentDef = mentDef,
            critRate = fromIntegral critTech / 3000,
            critDmg = 1.3 + fromIntegral critTech / 2000
        }
    getResonanceStatMod arc@(Arcanist {..}) = do
        stat360 <- getPlainStat $ arc {
            arcInsight = Insight 3,
            arcLevel = Level 60
        }
        Resonance.getStatMod @(ArcResType arc) arcResonance stat360

data SomeArcanist where
    SomeArcanist :: (IsArcanist a, IsResonanceType (ArcResType a)) => Arcanist a -> SomeArcanist
instance ArcanistInst SomeArcanist where
    getName (SomeArcanist arc) = getName arc
    getResonanceStatMod (SomeArcanist arc) = getResonanceStatMod arc

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
