{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.TwinSleep (
    TwinSleep
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data TwinSleep

instance IsArcanist TwinSleep where
    arcName = "Twins Sleep"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (28, 211, 320, 536, 799, undefined),
        hpGen = fromRaw (187, 1397, 2120, 3554, 5300, undefined),
        rdefGen = fromRaw (15, 115, 174, 292, 435, undefined),
        mdefGen = fromRaw (17, 130, 198, 331, 494, undefined),
        critGen = fromRaw (18, 139, 139, 157, 175, undefined)
    }

    type ArcResType TwinSleep = TypeU
