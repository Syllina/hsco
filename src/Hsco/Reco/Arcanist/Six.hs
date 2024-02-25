{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Six (
    Six
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Six

instance IsArcanist Six where
    arcName = "Six"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (33, 251, 381, 639, 952, 1120),
        hpGen = fromRaw (215, 1611, 2446, 4100, 6113, 7192),
        rdefGen = fromRaw (17, 132, 200, 335, 499, 587),
        mdefGen = fromRaw (18, 140, 213, 357, 531, 625),
        critGen = fromRaw (40, 300, 300, 340, 380, 420)
    }

    type ArcResType Six = TypeZ
