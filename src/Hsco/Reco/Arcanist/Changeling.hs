{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Changeling (
    Changeling
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Changeling

instance IsArcanist Changeling where
    arcName = "Changeling"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (34, 256, 389, 652, 972, 1143),
        hpGen = fromRaw (208, 1567, 2379, 3987, 5945, 6993),
        rdefGen = fromRaw (16, 121, 183, 307, 458, 538),
        mdefGen = fromRaw (16, 121, 183, 307, 458, 538),
        critGen = fromRaw (27, 204, 204, 231, 258, 285)
    }

    type ArcResType Changeling = TypeT
