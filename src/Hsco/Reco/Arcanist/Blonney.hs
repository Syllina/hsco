{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Blonney (
    Blonney
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Blonney

instance IsArcanist Blonney where
    arcName = "Blonney"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (31, 235, 356, 596, 889, 1046),
        hpGen = fromRaw (181, 1354, 2055, 3445, 5137, 6043),
        rdefGen = fromRaw (14, 109, 166, 277, 413, 486),
        mdefGen = fromRaw (13, 103, 157, 262, 391, 459),
        critGen = fromRaw (44, 331, 331, 375, 419, 463)
    }

    type ArcResType Blonney = TypeX
