{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Pavia (
    Pavia
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Pavia

instance IsArcanist Pavia where
    arcName = "Pavia"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (35, 264, 401, 672, 1002, undefined),
        hpGen = fromRaw (142, 1064, 1615, 2706, 4035, undefined),
        rdefGen = fromRaw (13, 99, 150, 252, 376, undefined),
        mdefGen = fromRaw (11, 83, 125, 210, 313, undefined),
        critGen = fromRaw (33, 250, 250, 283, 316, undefined)
    }

    type ArcResType Pavia = TypeT
