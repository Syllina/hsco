{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.ToothFairy (
    ToothFairy
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data ToothFairy

instance IsArcanist ToothFairy where
    arcName = "Tooth Fairy"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (33, 251, 381, 639, 952, 1120),
        hpGen = fromRaw (184, 1375, 2087, 3499, 5217, 6138),
        rdefGen = fromRaw (15, 116, 176, 294, 438, 515),
        mdefGen = fromRaw (17, 131, 198, 332, 495, 582),
        critGen = fromRaw (41, 313, 313, 354, 395, 436)
    }

    type ArcResType ToothFairy = TypeX
