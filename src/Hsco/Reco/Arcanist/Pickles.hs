{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Pickles (
    Pickles
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Pickles

instance IsArcanist Pickles where
    arcName = "Pickles"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (33, 251, 381, 639, 952, 1120),
        hpGen = fromRaw (194, 1449, 2199, 3687, 5498, 6468),
        rdefGen = fromRaw (15, 118, 180, 300, 447, 525),
        mdefGen = fromRaw (17, 133, 202, 338, 503, 592),
        critGen = fromRaw (29, 218, 218, 247, 276, 305)
    }

    type ArcResType Pickles = TypeZ
