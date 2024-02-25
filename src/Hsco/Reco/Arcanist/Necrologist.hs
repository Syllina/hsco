{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Necrologist (
    Necrologist
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Necrologist

instance IsArcanist Necrologist where
    arcName = "Necrologist"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 244, 370, 620, 924, 1086),
        hpGen = fromRaw (177, 1326, 2013, 3374, 5031, 5919),
        rdefGen = fromRaw (12, 92, 140, 234, 348, 410),
        mdefGen = fromRaw (17, 129, 195, 327, 488, 574),
        critGen = fromRaw (34, 254, 254, 288, 322, 356)
    }

    type ArcResType Necrologist = TypeT
