{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Satsuki (
    Satsuki
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Satsuki

instance IsArcanist Satsuki where
    arcName = "Satsuki"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 244, 370, 620, 924, 1086),
        hpGen = fromRaw (185, 1381, 2097, 3515, 5242, 6167),
        rdefGen = fromRaw (15, 115, 175, 292, 436, 512),
        mdefGen = fromRaw (12, 92, 140, 234, 348, 410),
        critGen = fromRaw (34, 254, 254, 288, 322, 356)
    }

    type ArcResType Satsuki = TypeX
