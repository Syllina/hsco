{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Lilya (
    Lilya
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Lilya

instance IsArcanist Lilya where
    arcName = "Lilya"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (33, 249, 378, 633, 944, 1110),
        hpGen = fromRaw (180, 1345, 2042, 3423, 5105, 6005),
        rdefGen = fromRaw (16, 123, 187, 313, 467, 549),
        mdefGen = fromRaw (13, 99, 150, 251, 374, 439),
        critGen = fromRaw (49, 368, 368, 417, 466, 515)
    }

    type ArcResType Lilya = TypeX
