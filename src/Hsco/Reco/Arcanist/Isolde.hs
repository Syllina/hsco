{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Isolde (
    Isolde
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Isolde

instance IsArcanist Isolde where
    arcName = "Isolde"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (33, 251, 381, 639, 952, 1120),
        hpGen = fromRaw (207, 1552, 2356, 3949, 5889, 6927),
        rdefGen = fromRaw (16, 126, 191, 319, 475, 559),
        mdefGen = fromRaw (15, 117, 178, 297, 443, 520),
        critGen = fromRaw (36, 273, 273, 309, 345, 381)
    }

    type ArcResType Isolde = TypeZ
