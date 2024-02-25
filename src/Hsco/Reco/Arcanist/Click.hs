{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Click (
    Click
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Click

instance IsArcanist Click where
    arcName = "Click"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (33, 248, 377, 632, 942, 1108),
        hpGen = fromRaw (203, 1520, 2307, 3866, 5765, 6783),
        rdefGen = fromRaw (16, 126, 192, 321, 478, 562),
        mdefGen = fromRaw (13, 101, 154, 257, 383, 450),
        critGen = fromRaw (27, 203, 203, 230, 257, 284)
    }

    type ArcResType Click = TypeZ
