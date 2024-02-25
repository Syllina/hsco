{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.DruvisIII (
    DruvisIII
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data DruvisIII

instance IsArcanist DruvisIII where
    arcName = "Druvis III"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (34, 254, 385, 646, 963, 1133),
        hpGen = fromRaw (190, 1419, 2154, 3611, 5385, 6336),
        rdefGen = fromRaw (15, 116, 176, 294, 438, 515),
        mdefGen = fromRaw (18, 136, 206, 345, 514, 605),
        critGen = fromRaw (29, 218, 218, 247, 276, 305)
    }

    type ArcResType DruvisIII = TypeZ
