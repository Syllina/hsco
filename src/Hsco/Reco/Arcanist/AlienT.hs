{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.AlienT (
    AlienT
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data AlienT

instance IsArcanist AlienT where
    arcName = "aliEn T"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (25, 190, 288, 482, 719, undefined),
        hpGen = fromRaw (209, 1562, 2370, 3974, 5926, undefined),
        rdefGen = fromRaw (16, 125, 190, 318, 474, undefined),
        mdefGen = fromRaw (12, 94, 143, 238, 355, undefined),
        critGen = fromRaw (23, 174, 174, 197, 220, undefined)
    }

    type ArcResType AlienT = TypeU
