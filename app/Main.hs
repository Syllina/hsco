module Main (main) where

-- import Hsco (fromRaw, genStatMaybe, Level(..), Insight(..), tmpAction)
import Hsco
import qualified Data.Text.IO as T

-- import Debug.Trace (trace)

-- 这段代码是用来计算共鸣主模块对应的属性加成的百分比的
-- 游戏内给出的主模块属性是数字，数字实际为
-- 对应角色洞三 60 数值乘上一个固定百分比的结果
-- 传入不同角色同一等级的主模块同一属性的列表 [(a, b)]
-- 其中 a 为模块属性加值，b 为对应属性洞三满级数值
calcCoef :: [(Int, Int)] -> (Int, Int)
-- for every pair (a, b)
-- a <= b * p / 1000 < a + 1
-- 1000a <= b * p < 1000(a + 1)
-- 1000a / b <= p < 1000(a + 1) / b
calcCoef = foldl' merge (0, 1000) . map getBound where
    merge (a, b) (c, d) = (max a c, min b d)
    getBound (a, b) = (ceiling $ 1000*a'/b', floor' $ 1000*(a'+1)/b') where
        a' = fromIntegral a :: Double
        b' = fromIntegral b :: Double
        floor' x = if x /= fromIntegral (floor x) then floor x else floor x - 1

showCoef :: [(Int, Int)] -> String
showCoef ls = "[" <> show l <> ", " <> show r <> "]" where
    (l, r) = calcCoef ls

verifyFormula :: [(Stat, RawDamage)] -> Either Text Stat
-- 接受 [(攻击者属性, 所造成伤害)]
-- 返回 Left "错误原因" / Right [可能的被攻击者属性]
--
-- 这实际上是一个线性规划问题
-- 待求解未知量为双防、受创减免、暴击防御
-- 由于解是离散的，暂且采用简单的枚举
-- （劣势在于开始无法用到暴击伤害的信息）
-- 首先枚举受创减免，假定其在 [0, 100.0%] 之间
-- 对于每个可能的值，通过对所有未暴击伤害枚举防御的方式
-- 判断是否可行
-- 若可行，进一步枚举暴击防御，并添加可能的解
verifyFormula dmgs = do
    let defRange = [0..1000]
        div1000 x = fromIntegral x / 1000
        drRange = map div1000 [0..1000]
        cdefRange = map div1000 [0..1000]

    let calcDmg src tar' (RawDamage{..}) = floor (atkTerm * bonusTerm * critTerm * mightTerm * affTerm * numTerm) where
            tar = tar' `applyMod` rdStatMod
            atkTerm = max (0.1 * fromIntegral (atk src)) ((fromIntegral $ atk src) - (fromIntegral $ def tar) * (1 - peneRate src))
            def = if fst rdType == Reality then realDef else mentDef
            bonusTerm = max 0.3 (1 + dmgBonus src - dmgReduct tar)
            getMight Incantation = incMight
            getMight Ritual = ritMight
            (dmgType, numTerm) = rdType
            mightTerm = 1 + (getMight rdSpell src)
            critTerm = critTerm' rdCrit
            critTerm' False = 1
            critTerm' True = max 1.1 (critDmg src - critDef tar)
            affTerm = if rdStronger then 1.3 else 1
        checkDmg src tar dmg@(RawDamage{..}) = rdValue == calcDmg src tar dmg

    let checkDR dr = flip all [Reality, Mental] $ \dt ->
                        flip any defRange $ \def ->
                        flip all (dmgs' dt) $ \(src, dmg) ->
                        checkDmg src (genStat dt dr def) dmg
        dmgs' dt = filter (\(_, x) -> fst (rdType x) == dt) $ filter (not.rdCrit.snd) dmgs
        genStat Reality dr def = statDef { dmgReduct = dr, realDef = def }
        genStat Mental dr def = statDef { dmgReduct = dr, mentDef = def }
    let debug1 = map (\(src, dmg) -> calcDmg src (genStat Reality 0.167 630) dmg) dmgs
        debug2 = flip all [Reality, Mental] $ \dt -> flip any defRange $ \def -> flip all dmgs $ \(src, dmg) -> checkDmg src (genStat dt 0.167 def) dmg
        debug3 = checkDR 0.167
        debugs = debug1 `traceShow` debug2 `traceShow` debug3
    let drs = filter checkDR drRange
    dr <- case drs of
            [] -> Left "在 [0, 100.0%] 内寻找不到有效的 <受创减免>，可能是数据或公式有误"
            x:[] -> Right x
            _ -> Left "在 [0, 100.0%] 内找到多个可能的 <受创减免>，请添加 *非暴击* 的测试数据"

    let checkDef dt def = flip all (dmgs' dt) $ \(src, dmg) ->
                            checkDmg src (genStat dt dr def) dmg
    let rdefs = filter (checkDef Reality) defRange
    rdef <- case rdefs of
            [] -> Left "在 [0, 1000] 内寻找不到有效的 <现实防御>，可能是数据或公式有误"
            x:[] -> Right x
            _ -> Left "在 [0, 1000] 内寻找到多个可能的 <现实防御>，请添加 *非暴击* 的 *现实伤害* 测试数据"
    let mdefs = filter (checkDef Mental) defRange
    mdef <- case mdefs of
            [] -> Left "在 [0, 1000] 内寻找不到有效的 <精神防御>，可能是数据或公式有误"
            x:[] -> Right x
            _ -> Left "在 [0, 1000] 内寻找到多个可能的 <精神防御>，请添加 *非暴击* 的 *精神伤害* 测试数据"

    let checkCritDef cdef = flip all (filter (rdCrit.snd) dmgs) $ \(src, dmg) ->
                                checkDmg src (genStat' cdef) dmg
        genStat' cdef = statDef { dmgReduct = dr, realDef = rdef, mentDef = mdef, critDef = cdef }
    let cdefs = filter checkCritDef cdefRange
    cdef <- case cdefs of
            [] -> Left "在 [0, 100.0%] 内寻找不到有效的 <暴击防御>，可能是数据或公式有误"
            x:[] -> Right x
            _ -> Left "在 [0, 100.0%] 内寻找到多个可能的 <暴击防御>，请添加 *暴击* 的测试数据"

    pure $ genStat' cdef

main :: IO ()
main = do
    -- let Just x = genStatMaybe (Insight 3, Level 37) (fromRaw (36, 269, 408, 683, 1019, 1199))
    -- print x
    -- let arcList = [SomeArcanist (Arcanist :: Arcanist ThirtySeven)]
    -- print $ map getName arcList

    -- putStrLn $ showCoef [(1757, 7031)]
    -- putStrLn $ showCoef [(212, 922)]
    -- putStrLn $ showCoef [(128, 534), (145, 605), (118, 492), (134, 559), (138, 579)]

    let arc = Arcanist {
        arcInsight = Insight 3,
        arcLevel = Level 37,
        arcResonance = Resonance {
            resLevel = 15,
            resActive = [(Double, 2), (O, 2), (L, 2), (J, 3), (S, 1), (T, 2), (BigX, 1)]
        },
        arcPsychube = SomePsychube $ (Psychube {
            psyAsc = 4,
            psyLevel = 60
        } :: Psychube SilentAndAdoring)
    } :: Arcanist ThirtySeven
    let Just stat = getPlainStat arc
        Just resStatMod = getResonanceStatMod arc
        Just psyStatMod = getPsychubeStatMod arc
    print $ applyMod stat $ resStatMod <> psyStatMod

    -- 2024.02 下半深眠 16-2 属性测试
    -- 洞三 20 共 10 梅兰妮，携带满增幅大娱乐
    let melania = statDef {
        atk = 1744,
        critDmg = 1.531 - 0.001,
        dmgBonus = 0.155 + 0.08,
        ritMight = 0.18
    }
    -- 既定计划
        buff11 k = statModDef { ritMightMod = 0.12 * k }
    -- 大娱乐被动
        buff12 k = statModDef { dmgBonusMod = 0.09 * k }
    -- 洞三 30 共 14 小鹿，携带满增幅夜色
    let jessica = statDef {
        atk = 1889,
        critDmg = 1.493 - 0.001,
        dmgBonus = 0.27 + 0.08,
        incMight = 0.18
    }
    -- 洞一被动 中毒增伤
        buff21 = statModDef { dmgBonusMod = 0.20 }
    -- 夜色被动
        buff22 = statModDef { dmgBonusMod = 0.24 }
    -- 洞三 20 共 10 可燃点，携带零增幅悄悄话
    let spathodea = statDef {
        atk = 1727,
        critDmg = 1.518 - 0.001,
        dmgBonus = 0.205 + 0.08,
        incMight = 0.18
    }
    -- 悄悄话被动一 双状态增益增伤
        buff31 = statModDef { dmgBonusMod = 0.08 }
    -- 悄悄话被动二 buff 卡叠层
        buff32 k = statModDef { atkRate = 0.02 * k }
    -- 单体卡额外倍率
        buff33 = statModDef { critDmgMod = 0.40 }
    -- 洞三 20 共 9 柏林，携带满增幅 51 级心驰神往
    let bkornblume = statDef {
        atk = floor $ 1488 * 1.05,
        critDmg = 1.397,
        dmgBonus = 0.13,
        incMight = 0.15
    }
    -- 洞一被动 debuff 增伤
        buff41 = statModDef { dmgBonusMod = 0.20 }
    -- debuff 卡
        buff42 1 = statModDef { realDefRate = -0.15, dmgReductMod = -0.15 }
        buff42 2 = statModDef { realDefRate = -0.20, dmgReductMod = -0.20 }
        buff42 3 = statModDef { realDefRate = -0.25, dmgReductMod = -0.25 }
        buff42 _  = statModDef
    -- 心驰神往被动
        buff43 = statModDef { dmgBonusMod = 0.16 }

    -- 模拟如下一段战斗
    either T.putStrLn print $ verifyFormula [
    -- 1/20
    -- 柏林 一阶 debuff
    -- 可燃点 一阶 buff
    -- 触发悄悄话被动 一层
    -- 可燃点 一阶单体
        (spathodea `applyMod` (buff31 <> buff32 1 <> buff33), rawDmgDef { rdType = (Reality, 2.00), rdStronger = True, rdStatMod = buff42 1, rdValue = 5066 }),
    -- 小鹿 一阶单体
    -- 一层中毒，触发夜色
        (jessica `applyMod` (buff21 <> buff22), rawDmgDef { rdType = (Reality, 2.20), rdStatMod = buff42 1, rdValue = 6227 }),
    -- 对方净化所有 debuff
    -- 柏林 一阶群攻
        (bkornblume, rawDmgDef { rdType = (Reality, 1.35), rdValue = 1393 }),
    -- 小鹿 二阶群攻
    -- 赋予一层中毒
        (jessica, rawDmgDef { rdType = (Reality, 2.00), rdValue = 3514 }),
    -- 可燃点 二阶单体
        (spathodea `applyMod` buff32 1, rawDmgDef { rdType = (Reality, 3.00), rdStronger = True, rdValue = 5819 }),
    -- 梅兰妮 一阶单体
        (melania, rawDmgDef { rdType = (Mental, 2.00), rdStronger = True, rdCrit = True, rdValue = 4111 }), -- 0.202
    -- 3/20 小鹿被动中毒
    -- 柏林 一阶 debuff
    -- 柏林 一阶群攻
        (bkornblume `applyMod` (buff41 <> buff43), rawDmgDef { rdType = (Reality, 1.80), rdStatMod = buff42 1, rdValue = 3128 }),
    -- 小鹿 一阶群攻
        (jessica `applyMod` buff21, rawDmgDef { rdType = (Reality, 1.65), rdStatMod = buff42 1, rdValue = 4038 }),
    -- 小鹿 一阶群攻
    -- 一致
    -- 4/20
    -- 柏林 一阶 debuff
    -- 小鹿仪式 两层毒
        (jessica, rawDmgDef { rdType = (Reality, 4.25), rdStatMod = buff42 1, rdSpell = Ritual, rdCrit = True, rdValue = 9895 }), -- 0.202
    -- 梅兰妮 一阶群攻
        (melania, rawDmgDef { rdType = (Mental, 1.20), rdStatMod = buff42 1, rdCrit = True, rdStronger = True, rdValue = 2813 }), -- 0.202
    -- 梅兰妮 一阶单体
        (melania, rawDmgDef { rdType = (Mental, 2.00), rdStatMod = buff42 1, rdCrit = True, rdStronger = True, rdValue = 4688 }), -- 0.202
    -- 5/20
    -- 柏林仪式
        (bkornblume, rawDmgDef { rdType = (Reality, 8.00), rdSpell = Ritual, rdCrit = True, rdValue = 8587 }), -- 0.201
    -- 可燃点 一阶 buff
    -- 可燃点 一阶单体
        (spathodea `applyMod` (buff31 <> buff32 2 <> buff33), rawDmgDef { rdType = (Reality, 2.00), rdStronger = True, rdCrit = True, rdValue = 7354 }), -- 0.202
    -- 小鹿 二阶单体
        (jessica, rawDmgDef { rdType = (Reality, 2.70), rdValue = 4745 }),
    -- 由于小梅一直在暴击，开第二把
    -- 1/20
    -- 柏林 debuff
    -- 小梅 群体
        (melania, rawDmgDef { rdType = (Mental, 1.20), rdStatMod = buff42 1, rdStronger = True, rdValue = 2116 }),
    -- 小梅 单体
        (melania, rawDmgDef { rdType = (Mental, 2.00), rdStatMod = buff42 1, rdStronger = True, rdValue = 3527 }),
    -- 小鹿 单体
        (jessica `applyMod` buff21, rawDmgDef { rdType = (Reality, 2.20), rdStatMod = buff42 1, rdCrit = True, rdValue = 6951 }), -- 0.202
    -- 2/20
        (jessica, rawDmgDef { rdType = (Reality, 1.35), rdCrit = True, rdValue = 3063 }), -- 0.202
        (melania, rawDmgDef { rdType = (Mental, 1.20), rdStronger = True, rdValue = 1856 }),
        (bkornblume `applyMod` (buff41 <> buff43), rawDmgDef { rdType = (Reality, 1.80), rdCrit = True, rdValue = 3052 }) -- 0.201
        ]
