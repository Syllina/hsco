{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.MatildaBouanich (
    MatildaBouanich
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data MatildaBouanich

instance IsArcanist MatildaBouanich where
    arcName = "Matilda Bouanich"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (31, 235, 356, 596, 889, 1046),
        hpGen = fromRaw (179, 1340, 2034, 3409, 5084, 5981),
        rdefGen = fromRaw (14, 108, 164, 275, 409, 481),
        mdefGen = fromRaw (16, 121, 183, 307, 458, 538),
        critGen = fromRaw (34, 254, 254, 288, 322, 356)
    }

    type ArcResType MatildaBouanich = TypeX
