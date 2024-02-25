{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Sputnik (
    Sputnik
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Sputnik

instance IsArcanist Sputnik where
    arcName = "Sputnik"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (26, 200, 304, 508, 758, undefined),
        hpGen = fromRaw (197, 1473, 2236, 3748, 5588, undefined),
        rdefGen = fromRaw (13, 100, 152, 255, 379, undefined),
        mdefGen = fromRaw (17, 130, 198, 331, 494, undefined),
        critGen = fromRaw (21, 162, 162, 183, 204, undefined)
    }

    type ArcResType Sputnik = TypeU
