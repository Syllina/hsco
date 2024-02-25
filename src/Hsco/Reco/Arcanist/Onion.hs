{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Onion (
    Onion
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Onion

instance IsArcanist Onion where
    arcName = "ONiON"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (31, 236, 358, 600, 894, undefined),
        hpGen = fromRaw (163, 1219, 1850, 3101, 4625, undefined),
        rdefGen = fromRaw (13, 99, 150, 252, 376, undefined),
        mdefGen = fromRaw (11, 83, 127, 212, 316, undefined),
        critGen = fromRaw (28, 213, 213, 241, 269, undefined)
    }

    type ArcResType Onion = TypeX
