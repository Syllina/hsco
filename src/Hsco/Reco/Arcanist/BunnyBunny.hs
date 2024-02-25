{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.BunnyBunny (
    BunnyBunny
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data BunnyBunny

instance IsArcanist BunnyBunny where
    arcName = "Bunny Bunny"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (31, 236, 358, 599, 893, undefined),
        hpGen = fromRaw (174, 1303, 1978, 3315, 4943, undefined),
        rdefGen = fromRaw (14, 109, 166, 277, 413, undefined),
        mdefGen = fromRaw (12, 91, 139, 232, 346, undefined),
        critGen = fromRaw (33, 250, 250, 283, 316, undefined)
    }

    type ArcResType BunnyBunny = TypeZ
