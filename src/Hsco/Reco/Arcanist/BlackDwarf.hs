{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.BlackDwarf (
    BlackDwarf
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data BlackDwarf

instance IsArcanist BlackDwarf where
    arcName = "Black Dwarf"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (34, 261, 396, 664, 989, 1163),
        hpGen = fromRaw (176, 1316, 1997, 3348, 4992, 5873),
        rdefGen = fromRaw (16, 123, 187, 313, 467, 549),
        mdefGen = fromRaw (14, 111, 168, 281, 419, 492),
        critGen = fromRaw (47, 354, 354, 401, 448, 495)
    }

    type ArcResType BlackDwarf = TypeZ
