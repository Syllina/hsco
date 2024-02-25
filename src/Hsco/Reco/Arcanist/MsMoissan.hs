{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.MsMoissan (
    MsMoissan
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data MsMoissan

instance IsArcanist MsMoissan where
    arcName = "Ms. Moissan"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (30, 231, 351, 588, 876, undefined),
        hpGen = fromRaw (176, 1316, 1998, 3349, 4994, undefined),
        rdefGen = fromRaw (14, 110, 167, 280, 416, undefined),
        mdefGen = fromRaw (16, 121, 184, 308, 459, undefined),
        critGen = fromRaw (16, 125, 125, 141, 157, undefined)
    }

    type ArcResType MsMoissan = TypeZ
