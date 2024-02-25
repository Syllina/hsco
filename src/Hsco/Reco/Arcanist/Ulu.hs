{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Ulu (
    Ulu
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Ulu

instance IsArcanist Ulu where
    arcName = "Ulu"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 241, 367, 614, 916, 1077),
        hpGen = fromRaw (206, 1538, 2334, 3913, 5835, 6864),
        rdefGen = fromRaw (17, 129, 196, 329, 490, 577),
        mdefGen = fromRaw (17, 128, 194, 326, 486, 572),
        critGen = fromRaw (33, 248, 248, 281, 314, 347)
    }

    type ArcResType Ulu = TypeT
