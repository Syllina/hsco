{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Melania (
    Melania
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Melania

instance IsArcanist Melania where
    arcName = "Melania"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (34, 254, 385, 646, 963, 1133),
        hpGen = fromRaw (186, 1390, 2109, 3536, 5273, 6204),
        rdefGen = fromRaw (17, 128, 194, 326, 486, 572),
        mdefGen = fromRaw (17, 128, 194, 326, 486, 572),
        critGen = fromRaw (32, 245, 245, 277, 309, 341)
    }

    type ArcResType Melania = TypeZ
