module Hsco.Reco.Stat where

class HasStat st where
    toStat :: st -> Stat

data Stat = Stat {
    atk :: Int,
    hp :: Int,
    realDef :: Int,
    mentDef :: Int,
    critRate :: Double,
    critRes :: Double,
    critDmg :: Double,
    critDef :: Double,
    dmgBonus :: Double,
    dmgReduct :: Double,
    dmgHeal :: Double,
    leechRate :: Double,
    incMight :: Double,
    ritMight :: Double,
    healRate :: Double,
    peneRate :: Double
} deriving (Show)

class HasStatMod stm where
    toStatMod :: stm -> StatMod

data StatMod = StatMod {
    atkMod, hpMod, realDefMod, mentDefMod :: Int,
    atkRate, hpRate, realDefRate, mentDefRate :: Double,
    critRateMod, critResMod, critDmgMod, critDefMod :: Double,
    dmgBonusMod, dmgReductMod, dmgHealMod :: Double,
    leechRateMod :: Double,
    incMightMod, ritMightMod :: Double,
    healRateMod :: Double,
    peneRateMod :: Double
} deriving (Show)

statDef :: Stat
statDef = Stat {
    atk = 0,
    hp = 0,
    realDef = 0,
    mentDef = 0,
    critRate = 0,
    critRes = 0,
    critDmg = 0,
    critDef = 0,
    dmgBonus = 0,
    dmgReduct = 0,
    dmgHeal = 0,
    leechRate = 0,
    incMight = 0,
    ritMight = 0,
    healRate = 0,
    peneRate = 0
}

statModDef :: StatMod
statModDef = StatMod {
    atkMod = 0,
    hpMod = 0,
    realDefMod = 0,
    mentDefMod = 0,
    atkRate = 0,
    hpRate = 0,
    realDefRate = 0,
    mentDefRate = 0,
    critRateMod = 0,
    critResMod = 0,
    critDmgMod = 0,
    critDefMod = 0,
    dmgBonusMod = 0,
    dmgReductMod = 0,
    dmgHealMod = 0,
    leechRateMod = 0,
    incMightMod = 0,
    ritMightMod = 0,
    healRateMod = 0,
    peneRateMod = 0
}

instance Semigroup StatMod where
    m1 <> m2 = StatMod {
        atkMod = atkMod m1 + atkMod m2,
        hpMod = hpMod m1 + hpMod m2,
        realDefMod = realDefMod m1 + realDefMod m2,
        mentDefMod = mentDefMod m1 + mentDefMod m2,
        atkRate = atkRate m1 + atkRate m2,
        hpRate = hpRate m1 + hpRate m2,
        realDefRate = realDefRate m1 + realDefRate m2,
        mentDefRate = mentDefRate m1 + mentDefRate m2,
        critRateMod = critRateMod m1 + critRateMod m2,
        critResMod = critResMod m1 + critResMod m2,
        critDmgMod = critDmgMod m1 + critDmgMod m2,
        critDefMod = critDefMod m1 + critDefMod m2,
        dmgBonusMod = dmgBonusMod m1 + dmgBonusMod m2,
        dmgReductMod = dmgReductMod m1 + dmgReductMod m2,
        dmgHealMod = dmgHealMod m1 + dmgHealMod m2,
        leechRateMod = leechRateMod m1 + leechRateMod m2,
        incMightMod = incMightMod m1 + incMightMod m2,
        ritMightMod = ritMightMod m1 + ritMightMod m2,
        healRateMod = healRateMod m1 + healRateMod m2,
        peneRateMod = peneRateMod m1 + peneRateMod m2
    }

-- 分别 apply 与 结合后 apply 结果不同
instance Monoid StatMod where
    mempty = statModDef

applyMod :: Stat -> StatMod -> Stat
applyMod (Stat {..}) (StatMod {..}) = Stat {
    atk = atkMod + floor (fromIntegral atk * (1 + atkRate)),
    hp = hpMod + floor (fromIntegral hp * (1 + hpRate)),
    -- temp change
    realDef = realDefMod + ceiling (fromIntegral realDef * (1 + realDefRate)),
    mentDef = mentDefMod + ceiling (fromIntegral mentDef * (1 + mentDefRate)),
    critRate = critRate + critRateMod,
    critRes = critRes + critResMod,
    critDmg = critDmg + critDmgMod,
    critDef = critDef + critDefMod,
    dmgBonus = dmgBonus + dmgBonusMod,
    dmgReduct = dmgReduct + dmgReductMod,
    dmgHeal = dmgHeal + dmgHealMod,
    leechRate = leechRate + leechRateMod,
    incMight = incMight + incMightMod,
    ritMight = ritMight + ritMightMod,
    healRate = healRate + healRateMod,
    peneRate = peneRate + peneRateMod
}

--- stat generator using linear interpolation {{{
linearInterpolate :: (Int, Int) -> (Int, Int) -> Int -> Double
linearInterpolate (x1', y1') (x2', y2') = \x -> (y2 - y1) / (x2 - x1) * (fromIntegral x - x1) + y1 where
    x1 = fromIntegral x1'
    y1 = fromIntegral y1'
    x2 = fromIntegral x2'
    y2 = fromIntegral y2'

--- }}}
