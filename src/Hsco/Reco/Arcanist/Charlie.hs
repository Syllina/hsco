{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Charlie (
    Charlie
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Charlie

instance IsArcanist Charlie where
    arcName = "Charlie"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (35, 264, 401, 673, 1003, 1179),
        hpGen = fromRaw (175, 1312, 1992, 3339, 4979, 5857),
        rdefGen = fromRaw (14, 109, 166, 277, 413, 486),
        mdefGen = fromRaw (12, 92, 140, 234, 348, 410),
        critGen = fromRaw (17, 129, 129, 146, 163, 180)
    }

    type ArcResType Charlie = TypeT
