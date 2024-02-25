{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Bkornblume (
    Bkornblume
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Bkornblume

instance IsArcanist Bkornblume where
    arcName = "Bkornblume"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (34, 255, 387, 649, 968, 1139),
        hpGen = fromRaw (177, 1326, 2013, 3374, 5031, 5919),
        rdefGen = fromRaw (13, 100, 152, 254, 379, 445),
        mdefGen = fromRaw (15, 115, 175, 292, 436, 512),
        critGen = fromRaw (18, 140, 140, 158, 176, 194)
    }

    type ArcResType Bkornblume = TypeZ
