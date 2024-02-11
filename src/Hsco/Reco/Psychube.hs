{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications, GADTs #-}
module Hsco.Reco.Psychube (
    SomePsychube(..),
    getStatMod,
    Psychube(..),
    SilentAndAdoring
) where

import Hsco.Reco.Stat

data PsyPlainData = PsyPlainData {
    atkGen, hpGen, rdefGen, mdefGen :: Int,
    -- 四次突破后对应的属性
    modGen :: (StatMod, StatMod, StatMod, StatMod)
}
class IsPsychube psy where
    psyName :: Text
    psyPlainData :: PsyPlainData

class PsychubeInst psy where
    getName :: psy -> Text
    getStatMod :: psy -> Maybe StatMod

-- newtype Ascension = Ascension Int
-- newtype Level = Level Int
data (IsPsychube psy) => Psychube psy = Psychube {
    -- ascension
    psyAsc :: Int,
    psyLevel :: Int
}
instance forall psy. IsPsychube psy => PsychubeInst (Psychube psy) where
    getName _ = psyName @psy
    getStatMod (Psychube {..}) = do
        let coef = fmap (\p -> p / 100) [15, 15.9, 16.8, 17.7, 18.6, 19.5, 20.4, 21.3, 22.2, 23, 23.9, 24.8, 25.7, 26.6, 27.5, 28.4, 29.3, 30.2, 31.1, 32, 33.3, 34.6, 35.9, 37.2, 38.5, 39.8, 41.1, 42.4, 43.7, 45, 46.3, 47.6, 48.9, 50.2, 51.5, 52.8, 54.1, 55.4, 56.7, 58, 60.1, 62.2, 64.3, 66.4, 68.5, 70.6, 72.7, 74.8, 76.9, 79, 81.1, 83.2, 85.3, 87.4, 89.5, 91.6, 93.7, 95.8, 97.9, 100]
        let usingGen gen = do
                p <- coef !!? (psyLevel-1)
                pure $ floor $ p * fromIntegral (gen $ psyPlainData @psy)
            (mod1, mod2, mod3, mod4) = modGen $ psyPlainData @psy
            usingAsc 1 = Just mod1
            usingAsc 2 = Just mod2
            usingAsc 3 = Just mod3
            usingAsc 4 = Just mod4
            usingAsc _ = Nothing
        atk <- usingGen atkGen
        hp <- usingGen hpGen
        realDef <- usingGen rdefGen
        mentDef <- usingGen mdefGen
        mod0 <- usingAsc psyAsc
        pure $ mod0 <> statModDef {
            atkMod = atk,
            hpMod = hp,
            realDefMod = realDef,
            mentDefMod = mentDef
        }

data SomePsychube where
    SomePsychube :: IsPsychube psy => Psychube psy -> SomePsychube
instance PsychubeInst SomePsychube where
    getName (SomePsychube psy) = getName psy
    getStatMod (SomePsychube psy) = getStatMod psy

tfor :: (a, a, a, a) -> (a -> b) -> (b, b, b, b)
tfor (a, b, c, d) f = (f a, f b, f c, f d)

data NoPsychube
instance IsPsychube NoPsychube where
    psyName = "无心相"
    psyPlainData = PsyPlainData {
        atkGen = 0,
        hpGen = 0,
        rdefGen = 0,
        mdefGen = 0,
        modGen = tfor (0, 0, 0, 0) $ const statModDef
    }

data SilentAndAdoring
instance IsPsychube SilentAndAdoring where
    psyName = "沉默与向往"
    psyPlainData = PsyPlainData {
        atkGen = 350,
        hpGen = 2000,
        rdefGen = 170,
        mdefGen = 170,
        modGen = tfor (0.04, 0.06, 0.08, 0.10) $ \p -> statModDef {
            atkRate = p
        }
    }
