{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.DarleyClatter (
    DarleyClatter
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data DarleyClatter

instance IsArcanist DarleyClatter where
    arcName = "Darley Clatter"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (29, 221, 336, 562, 838, undefined),
        hpGen = fromRaw (148, 1105, 1677, 2811, 4192, undefined),
        rdefGen = fromRaw (16, 123, 187, 313, 466, undefined),
        mdefGen = fromRaw (16, 123, 187, 313, 466, undefined),
        critGen = fromRaw (18, 139, 139, 157, 175, undefined)
    }

    type ArcResType DarleyClatter = TypeU
