{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Marcus (
    Marcus
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Marcus

instance IsArcanist Marcus where
    arcName = "Marcus"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (134, 1000, 1000, 1134, 1268, 1402),
        hpGen = fromRaw (134, 1000, 1000, 1134, 1268, 1402),
        rdefGen = fromRaw (134, 1000, 1000, 1134, 1268, 1402),
        mdefGen = fromRaw (134, 1000, 1000, 1134, 1268, 1402),
        critGen = fromRaw (134, 1000, 1000, 1134, 1268, 1402)
    }

    type ArcResType Marcus = TypeT
