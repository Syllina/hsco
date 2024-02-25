{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.AKnight (
    AKnight
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data AKnight

instance IsArcanist AKnight where
    arcName = "A Knight"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (35, 264, 400, 671, 1000, 1176),
        hpGen = fromRaw (217, 1626, 2468, 4137, 6169, 7258),
        rdefGen = fromRaw (16, 126, 191, 319, 475, 559),
        mdefGen = fromRaw (16, 126, 191, 319, 475, 559),
        critGen = fromRaw (36, 273, 273, 309, 345, 381)
    }

    type ArcResType AKnight = TypeZ
