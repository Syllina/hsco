{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Tennant (
    Tennant
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Tennant

instance IsArcanist Tennant where
    arcName = "Tennant"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 244, 370, 620, 924, 1086),
        hpGen = fromRaw (168, 1257, 1908, 3199, 4770, 5611),
        rdefGen = fromRaw (18, 135, 204, 342, 510, 601),
        mdefGen = fromRaw (12, 92, 140, 234, 348, 410),
        critGen = fromRaw (32, 242, 242, 274, 306, 338)
    }

    type ArcResType Tennant = TypeX
