{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Sonetto (
    Sonetto
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Sonetto

instance IsArcanist Sonetto where
    arcName = "Sonetto"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (29, 218, 332, 556, 829, 975),
        hpGen = fromRaw (203, 1520, 2307, 3866, 5765, 6783),
        rdefGen = fromRaw (17, 130, 197, 330, 492, 579),
        mdefGen = fromRaw (15, 115, 175, 292, 436, 512),
        critGen = fromRaw (23, 178, 178, 201, 224, 247)
    }

    type ArcResType Sonetto = TypeU
