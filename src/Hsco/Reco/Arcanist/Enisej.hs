{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Enisej (
    Enisej
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Enisej

instance IsArcanist Enisej where
    arcName = "Енисей"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (27, 207, 314, 526, 784, 922),
        hpGen = fromRaw (211, 1575, 2390, 4008, 5976, 7031),
        rdefGen = fromRaw (16, 120, 182, 304, 454, 534),
        mdefGen = fromRaw (16, 120, 182, 304, 454, 534),
        critGen = fromRaw (36, 275, 275, 311, 347, 383)
    }

    type ArcResType Enisej = TypeU
