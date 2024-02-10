{-# LANGUAGE DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, InstanceSigs #-}
module Hsco.Reco.Resonance (
    ResonanceType(..),
    Resonance(..)
) where

import Hsco.Reco.Stat

-- 共鸣块属性统计
-- 参考自灰机 wiki 及 nga  https://bbs.nga.cn/read.php?tid=37109986
--
-- 共鸣块名字
--      属性：(lvl) xx% -> (lvl) xx% -> ...
-- 增伤单格
--      生命：1%
--      创伤加成：0.5%
-- 减伤单格
--      生命：1%
--      受创减免：0.5%
-- 双格
--      暴击创伤：3% -> 4%
--      受创减免：1% -> 1%
-- 三格
--      攻击：1% -> 1% -> 1.5%
--      暴击率：1.5% -> 2% -> 2.5%
-- I 形
--      生命：2% -> 2.5% -> 3% -> 3.5%
--      攻击：1.5% -> 2.5% -> 3% -> 3.5%
--      精神防御：1.5% -> 2.5% -> 3% -> 3.5%
-- T 形
--      生命：2% -> 2.5% -> 3% -> 3.5%
--      攻击：1.5% -> 2.5% -> 3% -> 3.5%
--      现实防御：1.5% -> 2.5% -> 3% -> 3.5%
-- O 形
--      攻击：1.5% -> 2.5% -> 3% -> 3.5%
--      现实防御：1.5% -> 2.5% -> 3% -> 3.5%
--      精神防御：1.5% -> 2.5% -> 3% -> 3.5%
-- Z 形
--      抗暴率：3% -> 4% -> 5% -> 6%
--      暴击防御：3% -> 4% -> 5% -> 6%
--      受创减免：3% -> 4% -> 4.5% -> 5%
-- S 形
--      暴击率：1.5% -> 2% -> 2.5% -> 3%
--      抗暴率：1% -> 1.5% -> 2% -> 2%
--      创伤加成：2% -> 2.5% -> 3% -> 4%
-- J 形
--      暴击率：4% -> 5.5% -> 6% -> 7%
--      抗暴率：1% -> 1.5% -> 2% -> 2%
--      暴击创伤：2.5% -> 3% -> 3.5% -> 4%
-- L 形
--      暴击率：2.5% -> 3% -> 3.5% -> 4%
--      创伤加成：1.5% -> 2% -> 2.5% -> 3%
--      受创减免：1% -> 1.5% -> 2% -> 2.5%
--
-- 主模块的属性是按照洞三满级面板按百分比计算后向下取整得到的
-- 怀疑可能是按照那个隐藏数值？需要对所有角色进行验证，看看是否有不符合直接按面板算的情况存在（可以考虑分析低星角色对应的该值？）
-- 计算时假设了生命、现实防御与精神防御比例一直相等
-- 大十字 / 大 Z 形 / 大 T 形 的 生命 / 现防 / 精防都是一样的
-- 生命 / 现防 / 精防：1.4% -> 2.8% -> 5.6% -> 8.4% -> 11.2% -> 14.6% -> 17.9% -> 21.3% -> 24.6% -> 28.0%
-- 所有主模块的攻击都是一样的
-- 攻击：1.2% -> 2.3% -> 4.6% -> 6.9% -> 9.2% -> 12.0% -> 14.7% -> 17.5% -> 20.2% -> 23.0%
-- 所有主模块的这些子模块都是一样的
-- 类型：（共鸣等级）个数 * 等级
-- 增伤：(1)1*1 -> (2)2*1
-- 减伤：(1)1*1 -> (3)2*1
-- 双格：(1)1*1 -> (7)2*1 -> (12)3*2
-- 三格：(2)1*1 -> (4)2*1 -> (8)2*2 -> (9)3*2 -> (10)3*3 -> (11)4*3
-- I 形：(3)1*1 -> (5)1*2 -> (8)1*3 -> (9)2*3 -> (12)2*4
-- T 形：(3)1*1 -> (5)1*2 -> (8)1*3 -> (9)2*3 -> (12)2*4
-- O 形：(3)1*1 -> (5)1*2 -> (8)1*3 -> (9)2*3 -> (12)2*4
-- 这些子模块的顺序会随主模块变化，在下面称为“可变子模块”，按序给出
-- ? 形：(2)1*1 -> (4)2*1 -> (6)2*2 -> (8)3*2 -> (9)3*3 -> (10)4*3 -> (11)4*4
-- ? 形：(2)1*1 -> (6)1*2 -> (7)2*2 -> (10)2*3 -> (11)2*4
-- ? 形：(4)1*1 -> (7)1*2 -> (10)1*3 -> (13)1*4
-- ? 形：(5)1*1 -> (7)1*2 -> (10)1*3 -> (13)1*4
-- 大十字
--      暴击率：0 -> 4% -> 4% -> 5% -> 5% -> 6% -> 6% -> 7% -> 7% -> 8%
--      暴击创伤：0 -> 5% -> 5% -> 6% -> 7% -> 8% -> 10% -> 11% -> 13% -> 15%
--      抗暴率：0 -> 0 -> 0 -> 0 -> 0 -> 2% -> 3% -> 3% -> 3% -> 4%
--      受创减免：0 -> 0 -> 0 -> 0 -> 0 -> 0 -> 0 -> 1% -> 2% -> 3%
--      可变子模块：J L Z S
--
-- 大 T 形
--      创伤加成：0 -> 3% -> 3% -> 4% -> 5% -> 5% -> 7% -> 8% -> 9% -> 10%
--      抗暴率：0 -> 0 -> 2% -> 2% -> 2% -> 3% -> 4% -> 4% -> 4% -> 4%
--      暴击防御：0 -> 0 -> 0 -> 3% -> 3% -> 3% -> 4% -> 4% -> 4% -> 5%
--      暴击创伤：0 -> 0 -> 0 -> 0 -> 0 -> 0 -> 0 -> 4% -> 5% -> 6%
--      可变子模块：S L Z J
--
-- 大 Z 形
--      受创减免：0 -> 3% -> 3% -> 4% -> 4% -> 5% -> 6% -> 7% -> 7% -> 8%
--      创伤加成：0 -> 0 -> 1% -> 1% -> 2% -> 2% -> 3% -> 4% -> 4% -> 4%
--      抗暴率：0 -> 0 -> 0 -> 6% -> 8% -> 10% -> 10% -> 10% -> 10% -> 10%
--      暴击创伤：0 -> 0 -> 0 -> 0 -> 0 -> 6% -> 9% -> 12% -> 15% -> 18%
--      可变子模块：L S Z J
--
-- 大 U 形
--      生命：1.3% -> 2.5% -> 5.0% -> 7.5% -> 10.0% -> 13.0% -> 16.0% -> 19.0% -> 22.0% -> 25.0%
--      现防 / 精防：1.2% -> 2.4% -> 4.8% -> 7.2% -> 9.6% -> 12.5% -> 15.4% -> 18.2% -> 21.1% -> 24.0%
--      抗暴率：0 -> 6% -> 7% -> 8% -> 9% -> 10% -> 12% -> 13% -> 14% -> 16%
--      受创减免：0 -> 0 -> 1% -> 1% -> 2% -> 2% -> 2% -> 3% -> 4% -> 4%
--      创伤加成：0 -> 0 -> 0 -> 1% -> 1% -> 2% -> 2% -> 3% -> 3% -> 4%
--      暴击率：0 -> 0 -> 0 -> 0 -> 3% -> 3% -> 4% -> 5% -> 5% -> 6%
--      可变子模块：Z L S J

data ResonanceType = TypeX | TypeT | TypeZ | TypeU -- X as 十

class IsResonanceType (t :: ResonanceType) where
    getConstraints :: ()

data ResonanceBlockType = SingleDB | SingleDR | Double | Triple | I | T | O | S | L | Z | J | BigX | BigT | BigZ | BigU deriving (Eq)

data ResonanceBlock = ResonanceBlock {
    rbType :: ResonanceBlockType,
    rbLevel :: Int
}

instance HasStatMod ResonanceBlock where
    toStatMod :: ResonanceBlock -> StatMod
    toStatMod rb@(ResonanceBlock {..}) = case rbType of
        SingleDB | rbLevel == 1 -> statModDef { hpRate = 0.01, dmgBonusMod = 0.005 }

        SingleDR | rbLevel == 1 -> statModDef { hpRate = 0.01, dmgReductMod = 0.005 }

        Double | rbLevel == 1 -> statModDef { critDmgMod = 0.03, dmgReductMod = 0.01 }
               | rbLevel == 2 -> statModDef { critDmgMod = 0.04, dmgReductMod = 0.01 }

        Triple | rbLevel == 1 -> statModDef { atkRate = 0.010, critRateMod = 0.015 }
               | rbLevel == 2 -> statModDef { atkRate = 0.010, critRateMod = 0.020 }
               | rbLevel == 3 -> statModDef { atkRate = 0.015, critRateMod = 0.025 }

        I | rbLevel == 1 -> statModDef { hpRate = 0.020, atkRate = 0.015, mentDefRate = 0.015 }
          | rbLevel == 2 -> statModDef { hpRate = 0.025, atkRate = 0.025, mentDefRate = 0.025 }
          | rbLevel == 3 -> statModDef { hpRate = 0.030, atkRate = 0.030, mentDefRate = 0.030 }
          | rbLevel == 4 -> statModDef { hpRate = 0.035, atkRate = 0.035, mentDefRate = 0.035 }

        T | rbLevel == 1 -> statModDef { hpRate = 0.020, atkRate = 0.015, realDefRate = 0.015 }
          | rbLevel == 2 -> statModDef { hpRate = 0.025, atkRate = 0.025, realDefRate = 0.025 }
          | rbLevel == 3 -> statModDef { hpRate = 0.030, atkRate = 0.030, realDefRate = 0.030 }
          | rbLevel == 4 -> statModDef { hpRate = 0.035, atkRate = 0.035, realDefRate = 0.035 }

        O | rbLevel == 1 -> statModDef { atkRate = 0.015, realDefRate = 0.015, mentDefRate = 0.015 }
          | rbLevel == 2 -> statModDef { atkRate = 0.025, realDefRate = 0.025, mentDefRate = 0.025 }
          | rbLevel == 3 -> statModDef { atkRate = 0.030, realDefRate = 0.030, mentDefRate = 0.030 }
          | rbLevel == 4 -> statModDef { atkRate = 0.035, realDefRate = 0.035, mentDefRate = 0.035 }

        Z | rbLevel == 1 -> statModDef { critResMod = 0.03, critDefMod = 0.03, dmgReductMod = 0.030 }
          | rbLevel == 2 -> statModDef { critResMod = 0.04, critDefMod = 0.04, dmgReductMod = 0.040 }
          | rbLevel == 3 -> statModDef { critResMod = 0.05, critDefMod = 0.05, dmgReductMod = 0.045 }
          | rbLevel == 4 -> statModDef { critResMod = 0.06, critDefMod = 0.06, dmgReductMod = 0.050 }

        S | rbLevel == 1 -> statModDef { critRateMod = 0.015, critResMod = 0.010, dmgBonusMod = 0.020 }
          | rbLevel == 2 -> statModDef { critRateMod = 0.020, critResMod = 0.015, dmgBonusMod = 0.025 }
          | rbLevel == 3 -> statModDef { critRateMod = 0.025, critResMod = 0.020, dmgBonusMod = 0.030 }
          | rbLevel == 4 -> statModDef { critRateMod = 0.030, critResMod = 0.020, dmgBonusMod = 0.040 }

        J | rbLevel == 1 -> statModDef { critRateMod = 0.040, critResMod = 0.010, critDmgMod = 0.025 }
          | rbLevel == 2 -> statModDef { critRateMod = 0.055, critResMod = 0.015, critDmgMod = 0.030 }
          | rbLevel == 3 -> statModDef { critRateMod = 0.060, critResMod = 0.020, critDmgMod = 0.035 }
          | rbLevel == 4 -> statModDef { critRateMod = 0.070, critResMod = 0.020, critDmgMod = 0.040 }

        L | rbLevel == 1 -> statModDef { critRateMod = 0.025, dmgBonusMod = 0.015, dmgReductMod = 0.010 }
          | rbLevel == 2 -> statModDef { critRateMod = 0.030, dmgBonusMod = 0.020, dmgReductMod = 0.015 }
          | rbLevel == 3 -> statModDef { critRateMod = 0.035, dmgBonusMod = 0.025, dmgReductMod = 0.020 }
          | rbLevel == 4 -> statModDef { critRateMod = 0.040, dmgBonusMod = 0.030, dmgReductMod = 0.025 }

        _ | isMainBlock rb -> statModDef
        _ -> undefined

isMainBlock :: ResonanceBlock -> Bool
isMainBlock (ResonanceBlock {..}) = rbType == BigX || rbType == BigZ || rbType == BigT || rbType == BigU

getMainBlockMod :: ResonanceBlock -> Stat -> StatMod
getMainBlockMod rb _ | not $ isMainBlock rb = statModDef
getMainBlockMod (ResonanceBlock {..}) (Stat {..}) = mod1 <> mod2 <> mod3 where
    prop p attr = floor $ p * (fromIntegral attr)
    -- 这里用 case 是因为直接对 list 用 (!!) 应当会报 warning
    safeList xs func = case (xs !!? (rbLevel-1)) of
        Just p -> func p
        Nothing -> undefined
    -- 所有主模块的攻击百分比一致
    mod1 = safeList [0.012, 0.023, 0.046, 0.069, 0.092, 0.120, 0.147, 0.175, 0.202, 0.230] $ \p -> statModDef { atkMod = prop p atk }
    -- 除了大 U 块外，所有主模块的生命/现防/精防百分比一致且这三个百分比相等
    mod2 = case () of
        _ | rbType == BigX || rbType == BigZ || rbType == BigT ->
            safeList [0.014, 0.028, 0.056, 0.084, 0.112, 0.146, 0.179, 0.213, 0.246, 0.280] $ \p -> statModDef { hpMod = prop p hp, realDefMod = prop p realDef, mentDefMod = prop p mentDef }
        _ | rbType == BigU -> mod21 <> mod22 where
            mod21 = safeList [0.013, 0.025, 0.050, 0.075, 0.100, 0.130, 0.160, 0.190, 0.220, 0.250] $ \p -> statModDef { hpMod = prop p hp }
            mod22 = safeList [0.012, 0.024, 0.048, 0.072, 0.096, 0.125, 0.154, 0.182, 0.211, 0.240] $ \p -> statModDef { realDefMod = prop p realDef, mentDefMod = prop p mentDef }
        _ -> undefined
    -- 主模块独有的四个属性加成
    mod3 = case rbType of
        -- 大十字：暴击率 / 暴击创伤 / 抗暴率 / 受创减免
        BigX -> mod1 <> mod2 <> mod3 <> mod4 where
            mod1 = safeList [0, 0.04, 0.04, 0.05, 0.05, 0.06, 0.06, 0.07, 0.07, 0.08] $ \p -> statModDef { critRateMod = p }
            mod2 = safeList [0, 0.05, 0.05, 0.06, 0.07, 0.08, 0.10, 0.11, 0.13, 0.15] $ \p -> statModDef { critDmgMod = p }
            mod3 = safeList [0,    0,    0,    0,    0, 0.02, 0.03, 0.03, 0.03, 0.04] $ \p -> statModDef { critResMod = p }
            mod4 = safeList [0,    0,    0,    0,    0,    0,    0, 0.01, 0.02, 0.03] $ \p -> statModDef { dmgReductMod = p }
        -- 大 T 形：创伤加成 / 抗暴率 / 暴击防御 / 暴击创伤
        BigT -> mod1 <> mod2 <> mod3 <> mod4 where
            mod1 = safeList [0, 0.03, 0.03, 0.04, 0.05, 0.05, 0.07, 0.08, 0.09, 0.10] $ \p -> statModDef { dmgBonusMod = p }
            mod2 = safeList [0,    0, 0.02, 0.02, 0.02, 0.03, 0.04, 0.04, 0.04, 0.04] $ \p -> statModDef { critResMod = p }
            mod3 = safeList [0,    0,    0, 0.03, 0.03, 0.03, 0.04, 0.04, 0.04, 0.05] $ \p -> statModDef { critDefMod = p }
            mod4 = safeList [0,    0,    0,    0,    0,    0,    0, 0.04, 0.05, 0.06] $ \p -> statModDef { critDmgMod = p }
        -- 大 Z 形：受创减免 / 创伤加成 / 抗暴率 / 暴击创伤
        BigZ -> mod1 <> mod2 <> mod3 <> mod4 where
            mod1 = safeList [0, 0.03, 0.03, 0.04, 0.04, 0.05, 0.06, 0.07, 0.07, 0.08] $ \p -> statModDef { dmgReductMod = p }
            mod2 = safeList [0,    0, 0.01, 0.01, 0.02, 0.02, 0.03, 0.04, 0.04, 0.04] $ \p -> statModDef { dmgBonusMod = p }
            mod3 = safeList [0,    0,    0, 0.06, 0.08, 0.10, 0.10, 0.10, 0.10, 0.10] $ \p -> statModDef { critResMod = p }
            mod4 = safeList [0,    0,    0,    0,    0, 0.06, 0.09, 0.12, 0.15, 0.18] $ \p -> statModDef { critDmgMod = p }
        -- 大 U 形：抗暴率 / 受创减免 / 创伤加成 / 暴击率
        BigU -> mod1 <> mod2 <> mod3 <> mod4 where
            mod1 = safeList [0, 0.06, 0.07, 0.08, 0.09, 0.10, 0.12, 0.13, 0.14, 0.16] $ \p -> statModDef { critResMod = p }
            mod2 = safeList [0,    0, 0.01, 0.01, 0.02, 0.02, 0.02, 0.03, 0.04, 0.04] $ \p -> statModDef { dmgReductMod = p }
            mod3 = safeList [0,    0,    0, 0.01, 0.01, 0.02, 0.02, 0.03, 0.03, 0.04] $ \p -> statModDef { dmgBonusMod = p }
            mod4 = safeList [0,    0,    0,    0, 0.03, 0.03, 0.04, 0.05, 0.05, 0.06] $ \p -> statModDef { critRateMod = p }

        _ -> undefined

data Resonance (r :: ResonanceType) = Resonance {
    resLevel :: Int,
    resActive :: [ResonanceBlockType]
    -- 以及共鸣的摆放信息
}
