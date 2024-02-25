{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Diggers (
    Diggers
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Diggers

instance IsArcanist Diggers where
    arcName = "Diggers"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (31, 235, 356, 596, 889, 1046),
        hpGen = fromRaw (185, 1381, 2097, 3515, 5242, 6167),
        rdefGen = fromRaw (14, 110, 168, 280, 417, 490),
        mdefGen = fromRaw (16, 122, 185, 310, 462, 543),
        critGen = fromRaw (27, 203, 203, 230, 257, 284)
    }

    type ArcResType Diggers = TypeZ
