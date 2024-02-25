{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Spathodea (
    Spathodea
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Spathodea

instance IsArcanist Spathodea where
    arcName = "Spathodea"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (35, 266, 404, 677, 1009, 1186),
        hpGen = fromRaw (198, 1478, 2244, 3762, 5610, 6600),
        rdefGen = fromRaw (16, 121, 183, 307, 458, 538),
        mdefGen = fromRaw (14, 111, 168, 281, 419, 492),
        critGen = fromRaw (29, 218, 218, 247, 276, 305)
    }

    type ArcResType Spathodea = TypeT
