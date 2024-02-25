{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.TheFool (
    TheFool
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data TheFool

instance IsArcanist TheFool where
    arcName = "The Fool"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (26, 198, 301, 503, 750, undefined),
        hpGen = fromRaw (195, 1460, 2216, 3715, 5540, undefined),
        rdefGen = fromRaw (15, 112, 169, 284, 424, undefined),
        mdefGen = fromRaw (16, 125, 190, 318, 474, undefined),
        critGen = fromRaw (20, 151, 151, 171, 191, undefined)
    }

    type ArcResType TheFool = TypeU
