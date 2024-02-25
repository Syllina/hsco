{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.MesmerJr (
    MesmerJr
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data MesmerJr

instance IsArcanist MesmerJr where
    arcName = "Mesmer Jr."
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 245, 371, 622, 926, undefined),
        hpGen = fromRaw (160, 1197, 1816, 3045, 4540, undefined),
        rdefGen = fromRaw (14, 110, 167, 280, 416, undefined),
        mdefGen = fromRaw (17, 132, 201, 336, 500, undefined),
        critGen = fromRaw (36, 276, 276, 312, 348, undefined)
    }

    type ArcResType MesmerJr = TypeZ
