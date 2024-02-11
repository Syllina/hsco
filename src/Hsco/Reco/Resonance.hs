{-# LANGUAGE DataKinds, KindSignatures, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, InstanceSigs #-}
module Hsco.Reco.Resonance (
    ResonanceType(..),
    Resonance(..),
    getStatMod,
    IsResonanceType,
    ResonanceBlockType(..)
) where

import Hsco.Reco.Stat
import qualified Data.Map.Strict as Map
import Data.List (lookup)

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

data ResonanceBlockType = SingleDB | SingleDR | Double | Triple | I | T | O | S | L | Z | J | BigX | BigT | BigZ | BigU deriving (Eq, Ord)

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
    resActive :: [(ResonanceBlockType, Int)]
    -- 以及共鸣的摆放信息
}

class IsResonanceType (r :: ResonanceType) where
    getResBlockOrder :: (ResonanceBlockType, ResonanceBlockType, ResonanceBlockType, ResonanceBlockType, ResonanceBlockType)

    getLevelSize :: Int -> (Int, Int)
    getLevelSize = undefined

    -- (Int, Int) ==> (count, level)
    getLevelInfo :: Int -> Maybe (Map.Map ResonanceBlockType (Int, Int))
    getLevelInfo n | n < 1 || n > 15 = Nothing
    getLevelInfo n = Just $ loop n where
        loop 0 = Map.empty
        -- if dup, takes the latter one
        loop n = Map.unionWith (flip const) (loop (n-1)) (fromList $ curLevel n)

        (big, rb1, rb2, rb3, rb4) = getResBlockOrder @r
        curLevel 1 = [(big, (1, 1)), (SingleDB, (1, 1)), (SingleDR, (1, 1)), (Double, (1, 1))]
        curLevel 2 = [(big, (1, 2)), (SingleDB, (2, 1)), (Triple, (1, 1)), (rb1, (1, 1)), (rb2, (1, 1))]
        curLevel 3 = [(SingleDR, (2, 1)), (I, (1, 1)), (T, (1, 1)), (O, (1, 1))]
        curLevel 4 = [(big, (1, 3)), (Triple, (2, 1)), (rb1, (2, 1)), (rb3, (1, 1))]
        curLevel 5 = [(I, (1, 2)), (T, (1, 2)), (O, (1, 2)), (rb4, (1, 1))]
        curLevel 6 = [(big, (1, 4)), (rb1, (2, 2)), (rb2, (1, 2))]
        curLevel 7 = [(Double, (2, 1)), (rb2, (2, 2)), (rb3, (1, 2)), (rb4, (1, 2))]
        curLevel 8 = [(big, (1, 5)), (Triple, (2, 2)), (I, (1, 3)), (T, (1, 3)), (O, (1, 3)), (rb1, (3, 2))]
        curLevel 9 = [(Triple, (3, 2)), (I, (2, 3)), (T, (2, 3)), (O, (2, 3)), (rb1, (3, 3))]
        curLevel 10 = [(big, (1, 6)), (Triple, (3, 3)), (rb2, (2, 3)), (rb3, (1, 3)), (rb4, (1, 3))]
        curLevel 11 = [(big, (1, 7)), (Triple, (4, 3)), (rb1, (4, 4)), (rb2, (2, 4))]
        curLevel 12 = [(Double, (3, 2)), (I, (2, 4)), (T, (2, 4)), (O, (2, 4))]
        curLevel 13 = [(big, (1, 8)), (rb3, (1, 4)), (rb4, (1, 4))]
        curLevel 14 = [(big, (1, 9))]
        curLevel 15 = [(big, (1, 10))]

    getResonanceBlocks :: Resonance r -> Maybe [(ResonanceBlock, Int)]
    getResonanceBlocks (Resonance {..}) 
        | resLevel < 1 || resLevel > 15 = Nothing
        | Map.size rbMap == length resActive = result where
            rbMap :: Map.Map ResonanceBlockType Int
            rbMap = fromList resActive
            getBlock :: (ResonanceBlockType, Int) -> ResonanceBlock
            getBlock = undefined
            result = do
                levelInfo <- getLevelInfo @r resLevel
                let levelInfo' = Map.intersection levelInfo rbMap
                    diffMap = Map.differenceWith func levelInfo' rbMap
                    func (cap, lvl) cnt
                        | 0 <= cnt && cnt <= cap = Just (cnt, lvl)
                        | otherwise = Nothing
                guard (Map.size diffMap == Map.size rbMap)
                pure $ toList $ Map.mapWithKey (\rbt (cnt, lvl) -> (ResonanceBlock { rbType = rbt, rbLevel = lvl }, cnt)) diffMap

    getStatMod :: Resonance r -> Stat -> Maybe StatMod
    getStatMod res stat = do
        rbs <- getResonanceBlocks res
        let statMods = do
                (rb, cnt) <- rbs
                let mod | isMainBlock rb = getMainBlockMod rb stat
                        | otherwise = toStatMod rb
                pure $ replicate cnt mod
        pure $ mconcat $ mconcat statMods

instance IsResonanceType TypeX where
    getResBlockOrder = (BigX, J, L, Z, S)
instance IsResonanceType TypeT where
    getResBlockOrder = (BigT, S, L, Z, J)
instance IsResonanceType TypeZ where
    getResBlockOrder = (BigZ, L, S, Z, J)
instance IsResonanceType TypeU where
    getResBlockOrder = (BigU, Z, L, S, J)
