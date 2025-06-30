{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, KindSignatures, GADTs, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, QuantifiedConstraints, TypeFamilies, FlexibleContexts, UndecidableInstances, ConstraintKinds #-}
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
    module Hsco.Reco.Psychube,
    module Hsco.Reco.Stat
) where

import Hsco.Reco.Resonance hiding (getStatMod)
-- preventing possible name clashes (for getStatMod)
import qualified Hsco.Reco.Resonance as Resonance
import Hsco.Reco.Psychube hiding (getStatMod)
import qualified Hsco.Reco.Psychube as Psychube
import Hsco.Reco.Stat

-- generate arcanist plain stat using fixed stat
-- at certain levels which is given in the code
-- for each arcanist respectively
data ArcanistPlainData = ArcanistPlainData {
    atkGen, hpGen, rdefGen, mdefGen, critGen :: StatGenerator
}

-- this type class is intended to be implemented
-- for every arcanist (type)
class IsArcanist arc where
    -- name of the arcanist
    -- currently just a placeholder
    arcName :: Text
    -- constant stat at certain levels which
    -- is used to generate stat at all levels
    arcPlainData :: ArcanistPlainData
    -- 共鸣主模块类型
    type ArcResType arc :: ResonanceType

-- this is the type class of the arcanist instance
-- which has insight, level, etc.
class ArcanistInst arc where
    getName :: arc -> Text
    getPlainStat :: arc -> Maybe Stat
    -- 如果等级可以从类型上验证这里就不需要 Maybe 了
    getResonanceStatMod :: arc -> Maybe StatMod
    getPsychubeStatMod :: arc -> Maybe StatMod

type IsArcanist' arc = (IsArcanist arc, IsResonanceType (ArcResType arc))

-- this could be a kind!
newtype Insight = Insight Int deriving newtype (Eq, Num)
newtype Level = Level Int

data (IsArcanist arc) => Arcanist arc = Arcanist {
    arcInsight :: Insight,
    arcLevel :: Level,
    arcResonance :: Resonance (ArcResType arc),
    arcPsychube :: SomePsychube
}

instance forall arc. IsArcanist' arc => ArcanistInst (Arcanist arc) where
    getName _ = arcName @arc
    getPlainStat (Arcanist {..}) = do
        let usingGen gen = genStatMaybe (arcInsight, arcLevel) (gen $ arcPlainData @arc)
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
        stat360 <- getPlainStat $ (arc {
            arcInsight = Insight 3,
            arcLevel = Level 60
        } :: Arcanist arc)
        Resonance.getStatMod @(ArcResType arc) arcResonance stat360
    getPsychubeStatMod (Arcanist {..}) = Psychube.getStatMod arcPsychube

data SomeArcanist where
    SomeArcanist :: IsArcanist' arc => Arcanist arc -> SomeArcanist
instance ArcanistInst SomeArcanist where
    getName (SomeArcanist arc) = getName arc
    getResonanceStatMod (SomeArcanist arc) = getResonanceStatMod arc


data StatGenerator = StatGenerator {
    insight :: Int, -- insight bonus
    stat01 :: Int, -- stat at insight 0 level 1
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
