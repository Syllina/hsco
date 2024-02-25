{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Cristallo (
    Cristallo
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Cristallo

instance IsArcanist Cristallo where
    arcName = "Cristallo"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (30, 231, 351, 588, 876, undefined),
        hpGen = fromRaw (222, 1662, 2523, 4229, 6305, undefined),
        rdefGen = fromRaw (10, 77, 117, 196, 292, undefined),
        mdefGen = fromRaw (10, 77, 117, 196, 292, undefined),
        critGen = fromRaw (20, 150, 150, 170, 190, undefined)
    }

    type ArcResType Cristallo = TypeU
