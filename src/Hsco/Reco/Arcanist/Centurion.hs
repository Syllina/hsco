{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Centurion (
    Centurion
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Centurion

instance IsArcanist Centurion where
    arcName = "Centurion"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (34, 259, 393, 658, 980, 1153),
        hpGen = fromRaw (178, 1331, 2020, 3386, 5049, 5939),
        rdefGen = fromRaw (16, 123, 187, 313, 467, 549),
        mdefGen = fromRaw (13, 99, 150, 251, 374, 439),
        critGen = fromRaw (54, 409, 409, 463, 517, 571)
    }

    type ArcResType Centurion = TypeX
