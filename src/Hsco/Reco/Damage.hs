module Hsco.Reco.Damage where

import Hsco.Reco.Stat

data DamageType = Reality | Mental | Genesis deriving (Eq)
data SpellType = Incantation | Ritual deriving (Eq)

-- 区别于本源伤害附伤
-- 这是单纯的 ``一段伤害''
data RawDamage = RawDamage {
    rdType :: (DamageType, Double),
    rdCrit :: Bool,
    rdStronger :: Bool, -- 克制
    rdSpell :: SpellType,
    rdStatMod :: StatMod,
    rdValue :: Int
}

rawDmgDef :: RawDamage
rawDmgDef = RawDamage {
    rdType = (Genesis, 0),
    rdCrit = False,
    rdStronger = False,
    rdSpell = Incantation,
    rdStatMod = statModDef,
    rdValue = 0
}
