{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.LaSource (
    LaSource
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data LaSource

instance IsArcanist LaSource where
    arcName = "La Source"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (31, 232, 352, 590, 879, undefined),
        hpGen = fromRaw (161, 1206, 1831, 3069, 4576, undefined),
        rdefGen = fromRaw (13, 99, 150, 252, 376, undefined),
        mdefGen = fromRaw (13, 104, 158, 265, 394, undefined),
        critGen = fromRaw (18, 139, 139, 157, 175, undefined)
    }

    type ArcResType LaSource = TypeT
