{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.BabyBlue (
    BabyBlue
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data BabyBlue

instance IsArcanist BabyBlue where
    arcName = "Baby Blue"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (28, 214, 325, 544, 810, 953),
        hpGen = fromRaw (214, 1603, 2432, 4077, 6080, 7152),
        rdefGen = fromRaw (15, 112, 169, 284, 423, 498),
        mdefGen = fromRaw (18, 138, 209, 351, 523, 615),
        critGen = fromRaw (17, 127, 127, 144, 161, 178)
    }

    type ArcResType BabyBlue = TypeU
