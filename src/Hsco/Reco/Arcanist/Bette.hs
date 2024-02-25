{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Bette (
    Bette
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Bette

instance IsArcanist Bette where
    arcName = "Bette"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (28, 211, 320, 536, 799, undefined),
        hpGen = fromRaw (166, 1244, 1889, 3166, 4720, undefined),
        rdefGen = fromRaw (17, 130, 198, 331, 494, undefined),
        mdefGen = fromRaw (13, 99, 150, 252, 376, undefined),
        critGen = fromRaw (24, 185, 185, 209, 233, undefined)
    }

    type ArcResType Bette = TypeZ
