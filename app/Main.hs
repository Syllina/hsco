module Main (main) where

import Hsco
import qualified Data.Text as T
import qualified Data.Text.IO as T

import TextShow
import TextShow.Debug.Trace

import qualified Data.List.NonEmpty as NE

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

calcInsightBonus :: (Int, Int, Int, Int, Int, Int, Int, Int) -> Maybe Int
calcInsightBonus (d01, d030, d11, d140, d21, d250, d31, d360) = asum $ map checkDiff [1..1000] where
    stat diff = fromRaw (diff, d01, d030, d140, d250, d360)
    checkDiff diff 
        | genStatMaybe (1, Level 1) (stat diff) == Just d11 &&
          genStatMaybe (2, Level 1) (stat diff) == Just d21 &&
          (d31 == 0 || genStatMaybe (3, Level 1) (stat diff) == Just d31)
                 = Just diff
        | otherwise = Nothing
    

getData :: FilePath -> IO [(Text, [(Text, Int, Int, Int, Int, Int, Int)])]
getData fileName = do
    content <- readFileText fileName
    let groupedData = take 50 $ loop [] $ map (T.splitOn ";") $ lines content

        loop res [] = res
        loop res xs = (\(x, y) -> loop (res ++ [x]) y) $ splitAt 6 xs

        getJust (Just x) = x
        getJust Nothing = error "parse error"

        processItem cond items@[title@(name:_), atk, hp, rdef, mdef, tech] = getJust $ asum $ map (processAttr name cond) (drop 1 items)
        processItem _ _ = error "parse error"

        processAttr name cond [attr, t01, t030, t11, t140, t21, t250, t31, t360]
            | cond /= attr = Nothing
            | cond == attr && isJust diff = Just (name, fromMaybe (-1) diff, d01, d030, d140, d250, d360)
            | otherwise = Nothing
            where
                Just d01 = (readMaybe $ T.unpack t01) :: Maybe Int
                Just d030 = (readMaybe $ T.unpack t030) :: Maybe Int
                Just d11 = (readMaybe $ T.unpack t11) :: Maybe Int
                Just d140 = (readMaybe $ T.unpack t140) :: Maybe Int
                Just d21 = (readMaybe $ T.unpack t21) :: Maybe Int
                Just d250 = (readMaybe $ T.unpack t250) :: Maybe Int
                d31 = fromMaybe 0 (readMaybe $ T.unpack t31 :: Maybe Int)
                d360 = fromMaybe 0 (readMaybe $ T.unpack t360 :: Maybe Int)
                diff = calcInsightBonus (d01, d030, d11, d140, d21, d250, d31, d360)
        processAttr _ _ _ = Nothing

        attrs = ["攻击", "生命", "现实防御", "精神防御", "暴击技巧"]
        parseWithAttr attr = map (processItem attr) groupedData
        parsedData = flip map attrs $ \attr -> (attr, parseWithAttr attr)

    putTextLn $ showt $ length parsedData
    writeFileText "parsed.out" $ showt parsedData
    pure parsedData

-- (36, 269, 408, 683, 1019, 1199)

main :: IO ()
main = do

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

    arcData <- getData "result.out"
    let [(_, atk), (_, hp), (_, rdef), (_, mdef), (_, crit)] = arcData

    let atkAll360 = map (\(_, diff, _, _, _, _, d360) -> (diff, d360)) atk

    print $ map NE.head . NE.group . sort $ atkAll360

    pure ()
