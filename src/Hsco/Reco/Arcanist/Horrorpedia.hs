{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Horrorpedia (
    Horrorpedia
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Horrorpedia

instance IsArcanist Horrorpedia where
    arcName = "Horropedia"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (30, 230, 349, 584, 871, 1024),
        hpGen = fromRaw (194, 1451, 2202, 3691, 5504, 6475),
        rdefGen = fromRaw (13, 103, 157, 262, 391, 459),
        mdefGen = fromRaw (15, 115, 175, 292, 436, 512),
        critGen = fromRaw (30, 229, 229, 259, 289, 319)
    }

    type ArcResType Horrorpedia = TypeZ
