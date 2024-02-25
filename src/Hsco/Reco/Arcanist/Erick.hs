{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Erick (
    Erick
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Erick

instance IsArcanist Erick where
    arcName = "Erick"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 242, 368, 616, 919, undefined),
        hpGen = fromRaw (169, 1263, 1917, 3214, 4793, undefined),
        rdefGen = fromRaw (15, 116, 176, 294, 438, undefined),
        mdefGen = fromRaw (13, 99, 150, 252, 376, undefined),
        critGen = fromRaw (26, 200, 200, 226, 252, undefined)
    }

    type ArcResType Erick = TypeZ
