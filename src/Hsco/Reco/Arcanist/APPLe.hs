{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.APPLe (
    APPLe
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data APPLe

instance IsArcanist APPLe where
    arcName = "APPLe"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (30, 231, 351, 588, 876, undefined),
        hpGen = fromRaw (167, 1250, 1897, 3180, 4742, undefined),
        rdefGen = fromRaw (12, 95, 144, 240, 358, undefined),
        mdefGen = fromRaw (16, 126, 191, 319, 475, undefined),
        critGen = fromRaw (26, 200, 200, 226, 252, undefined)
    }

    type ArcResType APPLe = TypeT
