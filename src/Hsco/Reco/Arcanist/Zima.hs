{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Zima (
    Zima
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Zima

instance IsArcanist Zima where
    arcName = "Зима"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 242, 368, 616, 919, undefined),
        hpGen = fromRaw (169, 1263, 1917, 3214, 4793, undefined),
        rdefGen = fromRaw (14, 106, 161, 269, 401, undefined),
        mdefGen = fromRaw (14, 106, 161, 269, 401, undefined),
        critGen = fromRaw (23, 175, 175, 198, 221, undefined)
    }

    type ArcResType Zima = TypeT
