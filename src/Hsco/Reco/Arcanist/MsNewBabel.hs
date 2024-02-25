{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.MsNewBabel (
    MsNewBabel
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data MsNewBabel

instance IsArcanist MsNewBabel where
    arcName = "Ms. NewBabel"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (31, 232, 352, 589, 879, 1034),
        hpGen = fromRaw (227, 1700, 2581, 4326, 6450, 7588),
        rdefGen = fromRaw (18, 136, 206, 345, 514, 605),
        mdefGen = fromRaw (14, 111, 168, 281, 419, 492),
        critGen = fromRaw (29, 218, 218, 247, 276, 305)
    }

    type ArcResType MsNewBabel = TypeU
