{- |
Copyright: (c) 2023 Syllinxia
SPDX-License-Identifier: MIT
Maintainer: Syllinxia <syllinxia@gmail.com>

The very least comprehensive personal haskell tool
-}

module Hsco (
    module Hsco.Reco,
    someFunc
) where

import Hsco.Reco

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
