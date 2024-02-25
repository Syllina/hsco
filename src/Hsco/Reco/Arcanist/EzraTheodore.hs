{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.EzraTheodore (
    EzraTheodore
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data EzraTheodore

instance IsArcanist EzraTheodore where
    arcName = "Ezra Theodore"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (28, 212, 322, 539, 803, 945),
        hpGen = fromRaw (247, 1848, 2805, 4702, 7012, 8249),
        rdefGen = fromRaw (17, 128, 194, 326, 486, 572),
        mdefGen = fromRaw (17, 128, 194, 326, 486, 572),
        critGen = fromRaw (43, 327, 327, 370, 413, 456)
    }

    type ArcResType EzraTheodore = TypeX
