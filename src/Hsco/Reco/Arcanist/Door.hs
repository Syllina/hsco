{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Door (
    Door
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Door

instance IsArcanist Door where
    arcName = "Door"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (28, 211, 320, 536, 799, undefined),
        hpGen = fromRaw (170, 1270, 1927, 3231, 4818, undefined),
        rdefGen = fromRaw (14, 110, 166, 278, 414, undefined),
        mdefGen = fromRaw (14, 110, 166, 278, 414, undefined),
        critGen = fromRaw (23, 178, 178, 201, 224, undefined)
    }

    type ArcResType Door = TypeZ
