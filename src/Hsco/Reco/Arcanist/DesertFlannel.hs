{-# LANGUAGE DataKinds, TypeFamilies #-}
module Hsco.Reco.Arcanist.DesertFlannel (
    DesertFlannel
) where

import Hsco.Reco.Resonance (ResonanceType(..))
import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data DesertFlannel

instance IsArcanist DesertFlannel where
    arcName = "Desert Flannel"
    arcPlainData = ArcanistPlainData {
        atkGen = fromRaw (30, 230, 349, 584, 871, 1024),
        hpGen = fromRaw (185, 1381, 2097, 3515, 5242, 6167),
        rdefGen = fromRaw (16, 122, 185, 310, 462, 543),
        mdefGen = fromRaw (16, 120, 182, 304, 454, 534),
        critGen = fromRaw (30, 231, 231, 261, 291, 321)
    }

    type ArcResType DesertFlannel = TypeZ
