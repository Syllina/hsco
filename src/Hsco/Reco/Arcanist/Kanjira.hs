{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Kanjira (
    Kanjira
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Kanjira

instance IsArcanist Kanjira where
    arcName = "Kanjira"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (31, 237, 360, 602, 897, 1055),
        hpGen = fromRaw (175, 1312, 1992, 3339, 4979, 5857),
        rdefGen = fromRaw (15, 112, 169, 284, 423, 498),
        mdefGen = fromRaw (16, 120, 182, 304, 454, 534),
        critGen = fromRaw (26, 196, 196, 222, 248, 274)
    }

    type ArcResType Kanjira = TypeZ
