{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.OliverFog (
    OliverFog
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data OliverFog

instance IsArcanist OliverFog where
    arcName = "Oliver Fog"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 242, 368, 616, 919, undefined),
        hpGen = fromRaw (160, 1197, 1816, 3045, 4540, undefined),
        rdefGen = fromRaw (17, 132, 201, 336, 500, undefined),
        mdefGen = fromRaw (11, 88, 134, 223, 333, undefined),
        critGen = fromRaw (20, 150, 150, 170, 190, undefined)
    }

    type ArcResType OliverFog = TypeZ
