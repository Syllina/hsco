{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.AnAnLee (
    AnAnLee
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data AnAnLee

instance IsArcanist AnAnLee where
    arcName = "An-an Lee"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (34, 261, 396, 664, 989, 1163),
        hpGen = fromRaw (168, 1257, 1907, 3197, 4768, 5609),
        rdefGen = fromRaw (17, 128, 194, 326, 486, 572),
        mdefGen = fromRaw (17, 128, 194, 326, 486, 572),
        critGen = fromRaw (36, 273, 273, 309, 345, 381)
    }

    type ArcResType AnAnLee = TypeT
