{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.BalloonParty (
    BalloonParty
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data BalloonParty

instance IsArcanist BalloonParty where
    arcName = "Balloon Party"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (29, 218, 332, 556, 829, 975),
        hpGen = fromRaw (203, 1520, 2307, 3866, 5765, 6783),
        rdefGen = fromRaw (19, 144, 218, 366, 545, 641),
        mdefGen = fromRaw (14, 106, 161, 269, 401, 472),
        critGen = fromRaw (22, 170, 170, 192, 214, 236)
    }

    type ArcResType BalloonParty = TypeU
