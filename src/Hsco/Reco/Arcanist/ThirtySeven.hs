{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.ThirtySeven (
    ThirtySeven
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data ThirtySeven

instance IsArcanist ThirtySeven where
    arcName = "Thirty-seven"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (36, 269, 408, 683, 1019, 1199),
        hpGen = fromRaw (166, 1242, 1885, 3160, 4712, 5543),
        rdefGen = fromRaw (15, 113, 172, 288, 430, 505),
        mdefGen = fromRaw (17, 131, 198, 332, 495, 582),
        critGen = fromRaw (41, 313, 313, 354, 395, 436)
    }

    type ArcResType ThirtySeven = TypeX
