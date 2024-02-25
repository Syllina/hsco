{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Dikke (
    Dikke
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Dikke

instance IsArcanist Dikke where
    arcName = "Dikke"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (31, 235, 356, 596, 889, 1046),
        hpGen = fromRaw (185, 1381, 2097, 3515, 5242, 6167),
        rdefGen = fromRaw (14, 106, 161, 269, 401, 472),
        mdefGen = fromRaw (16, 124, 188, 315, 470, 553),
        critGen = fromRaw (28, 216, 216, 244, 272, 300)
    }

    type ArcResType Dikke = TypeZ
