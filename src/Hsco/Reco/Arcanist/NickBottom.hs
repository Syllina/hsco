{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.NickBottom (
    NickBottom
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data NickBottom

instance IsArcanist NickBottom where
    arcName = "Nick Bottom"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (26, 200, 304, 509, 759, undefined),
        hpGen = fromRaw (213, 1596, 2422, 4059, 6053, undefined),
        rdefGen = fromRaw (17, 132, 201, 336, 500, undefined),
        mdefGen = fromRaw (13, 99, 150, 252, 376, undefined),
        critGen = fromRaw (23, 175, 175, 198, 221, undefined)
    }

    type ArcResType NickBottom = TypeU
