{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.GeTian (
    GeTian
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data GeTian

instance IsArcanist GeTian where
    arcName = "Getian"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 246, 374, 626, 933, 1097),
        hpGen = fromRaw (206, 1538, 2334, 3913, 5835, 6864),
        rdefGen = fromRaw (16, 124, 189, 316, 471, 554),
        mdefGen = fromRaw (16, 124, 189, 316, 471, 554),
        critGen = fromRaw (29, 218, 218, 247, 276, 305)
    }

    type ArcResType GeTian = TypeT
