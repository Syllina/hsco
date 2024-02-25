{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Rabies (
    Rabies
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Rabies

instance IsArcanist Rabies where
    arcName = "Rabies"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (29, 220, 334, 560, 835, undefined),
        hpGen = fromRaw (204, 1529, 2321, 3890, 5801, undefined),
        rdefGen = fromRaw (13, 99, 150, 252, 376, undefined),
        mdefGen = fromRaw (13, 99, 150, 252, 376, undefined),
        critGen = fromRaw (26, 200, 200, 226, 252, undefined)
    }

    type ArcResType Rabies = TypeT
