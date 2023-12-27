{- |
Copyright: (c) 2023 Syllinxia
SPDX-License-Identifier: MIT
Maintainer: Syllinxia <syllinxia@gmail.com>

The very least comprehensive personal haskell tool
-}

module Hsco
       ( someFunc,
         module Hsco.Thu
       ) where

import Hsco.Thu


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
