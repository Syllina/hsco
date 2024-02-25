{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.MedicinePocket (
    MedicinePocket
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data MedicinePocket

instance IsArcanist MedicinePocket where
    arcName = "Medicine Pocket"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 241, 367, 614, 916, 1077),
        hpGen = fromRaw (202, 1508, 2289, 3837, 5722, 6732),
        rdefGen = fromRaw (16, 126, 191, 319, 475, 559),
        mdefGen = fromRaw (16, 126, 191, 319, 475, 559),
        critGen = fromRaw (36, 273, 273, 309, 345, 381)
    }

    type ArcResType MedicinePocket = TypeU
