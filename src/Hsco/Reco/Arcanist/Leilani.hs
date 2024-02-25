{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Leilani (
    Leilani
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Leilani

instance IsArcanist Leilani where
    arcName = "Leilani"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (31, 234, 355, 595, 887, undefined),
        hpGen = fromRaw (148, 1105, 1677, 2811, 4192, undefined),
        rdefGen = fromRaw (14, 110, 166, 278, 414, undefined),
        mdefGen = fromRaw (13, 99, 150, 252, 376, undefined),
        critGen = fromRaw (23, 176, 176, 199, 222, undefined)
    }

    type ArcResType Leilani = TypeZ
