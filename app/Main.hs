module Main (main) where

-- import Hsco (fromRaw, genStatMaybe, Level(..), Insight(..), tmpAction)
import Hsco

main :: IO ()
main = do
    let Just x = genStatMaybe (Insight 3, Level 37) (fromRaw (36, 269, 408, 683, 1019, 1199))
    print x
    let arcList = [SomeArcanist (Arcanist :: Arcanist ThirtySeven)]
    print $ map getName arcList
