{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Schneider (
    Schneider
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Schneider

instance IsArcanist Schneider where
    arcName = "Schneider"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 241, 367, 614, 916, 1077),
        hpGen = fromRaw (168, 1257, 1908, 3199, 4770, 5611),
        rdefGen = fromRaw (13, 103, 157, 262, 391, 459),
        mdefGen = fromRaw (13, 103, 157, 262, 391, 459),
        critGen = fromRaw (47, 356, 356, 403, 450, 497)
    }

    type ArcResType Schneider = TypeX
