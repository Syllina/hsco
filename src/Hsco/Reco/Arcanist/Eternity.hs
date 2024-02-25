{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Eternity (
    Eternity
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Eternity

instance IsArcanist Eternity where
    arcName = "Eternity"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (33, 251, 381, 639, 952, 1120),
        hpGen = fromRaw (206, 1538, 2334, 3913, 5835, 6864),
        rdefGen = fromRaw (18, 136, 206, 345, 514, 605),
        mdefGen = fromRaw (14, 111, 168, 281, 419, 492),
        critGen = fromRaw (24, 182, 182, 206, 230, 254)
    }

    type ArcResType Eternity = TypeZ
