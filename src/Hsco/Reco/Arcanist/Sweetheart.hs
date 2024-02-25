{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Sweetheart (
    Sweetheart
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Sweetheart

instance IsArcanist Sweetheart where
    arcName = "Sweetheart"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 241, 367, 614, 916, 1077),
        hpGen = fromRaw (175, 1312, 1992, 3339, 4979, 5857),
        rdefGen = fromRaw (15, 115, 175, 292, 436, 512),
        mdefGen = fromRaw (12, 92, 140, 234, 348, 410),
        critGen = fromRaw (47, 356, 356, 403, 450, 497)
    }

    type ArcResType Sweetheart = TypeX
