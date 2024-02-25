{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Voyager (
    Voyager
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Voyager

instance IsArcanist Voyager where
    arcName = "Voyager"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (33, 251, 381, 639, 952, 1120),
        hpGen = fromRaw (202, 1508, 2289, 3837, 5722, 6732),
        rdefGen = fromRaw (15, 116, 176, 294, 438, 515),
        mdefGen = fromRaw (16, 123, 187, 313, 467, 549),
        critGen = fromRaw (31, 234, 234, 265, 296, 327)
    }

    type ArcResType Voyager = TypeZ
