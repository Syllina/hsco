{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.JohnTitor (
    JohnTitor
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data JohnTitor

instance IsArcanist JohnTitor where
    arcName = "John Titor"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (31, 232, 352, 590, 879, undefined),
        hpGen = fromRaw (168, 1257, 1908, 3198, 4769, undefined),
        rdefGen = fromRaw (13, 99, 150, 252, 376, undefined),
        mdefGen = fromRaw (15, 115, 174, 292, 435, undefined),
        critGen = fromRaw (24, 185, 185, 209, 233, undefined)
    }

    type ArcResType JohnTitor = TypeZ
