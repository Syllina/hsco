{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.MsRadio (
    MsRadio
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data MsRadio

instance IsArcanist MsRadio where
    arcName = "Ms. Radio"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (29, 217, 329, 552, 823, undefined),
        hpGen = fromRaw (178, 1333, 2024, 3392, 5058, undefined),
        rdefGen = fromRaw (13, 104, 158, 265, 394, undefined),
        mdefGen = fromRaw (13, 104, 158, 265, 394, undefined),
        critGen = fromRaw (29, 223, 223, 252, 281, undefined)
    }

    type ArcResType MsRadio = TypeU
