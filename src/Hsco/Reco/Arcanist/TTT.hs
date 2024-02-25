{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.TTT (
    TTT
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data TTT

instance IsArcanist TTT where
    arcName = "TTT"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (29, 220, 334, 560, 835, undefined),
        hpGen = fromRaw (178, 1330, 2018, 3383, 5045, undefined),
        rdefGen = fromRaw (14, 110, 167, 280, 416, undefined),
        mdefGen = fromRaw (16, 121, 184, 308, 459, undefined),
        critGen = fromRaw (26, 200, 200, 226, 252, undefined)
    }

    type ArcResType TTT = TypeZ
