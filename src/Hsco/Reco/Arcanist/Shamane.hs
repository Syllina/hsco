{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Shamane (
    Shamane
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Shamane

instance IsArcanist Shamane where
    arcName = "Shamane"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (33, 249, 378, 633, 944, 1110),
        hpGen = fromRaw (217, 1626, 2468, 4137, 6169, 7258),
        rdefGen = fromRaw (15, 117, 178, 297, 443, 520),
        mdefGen = fromRaw (15, 113, 172, 288, 430, 505),
        critGen = fromRaw (25, 191, 191, 216, 241, 266)
    }

    type ArcResType Shamane = TypeZ
