{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.JiuNiangZi (
    JiuNiangZi
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data JiuNiangZi

instance IsArcanist JiuNiangZi where
    arcName = "Jiu Niangzi"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (34, 259, 393, 658, 980, 1153),
        hpGen = fromRaw (200, 1493, 2266, 3800, 5666, 6666),
        rdefGen = fromRaw (15, 113, 172, 288, 430, 505),
        mdefGen = fromRaw (14, 106, 161, 269, 402, 472),
        critGen = fromRaw (51, 382, 382, 433, 484, 535)
    }

    type ArcResType JiuNiangZi = TypeZ
