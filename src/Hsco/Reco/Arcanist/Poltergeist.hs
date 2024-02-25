{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Poltergeist (
    Poltergeist
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Poltergeist

instance IsArcanist Poltergeist where
    arcName = "Poltergeist"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 245, 371, 622, 926, undefined),
        hpGen = fromRaw (160, 1197, 1816, 3045, 4540, undefined),
        rdefGen = fromRaw (19, 143, 217, 364, 543, undefined),
        mdefGen = fromRaw (14, 110, 167, 280, 416, undefined),
        critGen = fromRaw (23, 175, 175, 198, 221, undefined)
    }

    type ArcResType Poltergeist = TypeU
