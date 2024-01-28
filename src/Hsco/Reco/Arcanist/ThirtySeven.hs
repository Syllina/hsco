module Hsco.Reco.Arcanist.ThirtySeven (
    ThirtySeven
) where

import Hsco.Reco.Arcanist (IsArcanist(..), ArcanistPlainData(..), fromRaw)

data ThirtySeven

instance IsArcanist ThirtySeven where
    arcName = "Thirty-seven"
    arcPlainStat = ArcanistPlainData {
        atk = fromRaw (36, 269, 408, 683, 1019, 1199),
        hp = fromRaw (166, 1242, 1885, 3160, 4712, 5543),
        rdef = fromRaw (15, 113, 172, 288, 430, 505),
        mdef = fromRaw (17, 131, 198, 332, 495, 582),
        crit = fromRaw (41, 313, 313, 354, 395, 436)
    }
