{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Regulus (
    Regulus
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Regulus

instance IsArcanist Regulus where
    arcName = "Regulus"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (35, 266, 404, 677, 1009, 1186),
        hpGen = fromRaw (168, 1257, 1907, 3197, 4768, 5609),
        rdefGen = fromRaw (13, 101, 153, 257, 382, 449),
        mdefGen = fromRaw (18, 136, 206, 345, 514, 605),
        critGen = fromRaw (49, 368, 368, 417, 466, 515)
    }

    type ArcResType Regulus = TypeT
