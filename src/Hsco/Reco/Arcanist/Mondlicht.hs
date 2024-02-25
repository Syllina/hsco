{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Mondlicht (
    Mondlicht
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Mondlicht

instance IsArcanist Mondlicht where
    arcName = "Mondlicht"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 242, 368, 616, 919, undefined),
        hpGen = fromRaw (165, 1237, 1877, 3146, 4691, undefined),
        rdefGen = fromRaw (13, 99, 150, 252, 376, undefined),
        mdefGen = fromRaw (16, 121, 184, 308, 459, undefined),
        critGen = fromRaw (20, 150, 150, 170, 190, undefined)
    }

    type ArcResType Mondlicht = TypeZ
