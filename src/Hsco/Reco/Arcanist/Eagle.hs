{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Eagle (
    Eagle
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Eagle

instance IsArcanist Eagle where
    arcName = "Eagle"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 242, 368, 616, 919, undefined),
        hpGen = fromRaw (160, 1197, 1816, 3045, 4540, undefined),
        rdefGen = fromRaw (14, 110, 167, 280, 416, undefined),
        mdefGen = fromRaw (10, 77, 117, 196, 292, undefined),
        critGen = fromRaw (47, 351, 351, 398, 445, undefined)
    }

    type ArcResType Eagle = TypeX
