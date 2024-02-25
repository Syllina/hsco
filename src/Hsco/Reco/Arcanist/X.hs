{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.X (
    X
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data X

instance IsArcanist X where
    arcName = "X"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 241, 367, 614, 916, 1077),
        hpGen = fromRaw (203, 1520, 2307, 3866, 5765, 6783),
        rdefGen = fromRaw (16, 121, 183, 307, 458, 538),
        mdefGen = fromRaw (16, 121, 183, 307, 458, 538),
        critGen = fromRaw (27, 203, 203, 230, 257, 284)
    }

    type ArcResType X = TypeZ
