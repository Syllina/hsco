{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.Sotheby (
    Sotheby
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data Sotheby

instance IsArcanist Sotheby where
    arcName = "Sotheby"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (32, 246, 374, 626, 933, 1097),
        hpGen = fromRaw (221, 1656, 2513, 4213, 6282, 7390),
        rdefGen = fromRaw (15, 116, 176, 294, 438, 515),
        mdefGen = fromRaw (15, 116, 176, 294, 438, 515),
        critGen = fromRaw (27, 204, 204, 231, 258, 285)
    }

    type ArcResType Sotheby = TypeZ
