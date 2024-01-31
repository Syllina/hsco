module Main (main) where

-- import Hsco (fromRaw, genStatMaybe, Level(..), Insight(..), tmpAction)
import Hsco

calcCoef :: [(Int, Int)] -> (Int, Int)
-- for every pair (a, b)
-- a <= b * p / 1000 < a + 1
-- 1000a <= b * p < 1000(a + 1)
-- 1000a / b <= p < 1000(a + 1) / b
calcCoef = foldl' merge (0, 1000) . map getBound where
    merge (a, b) (c, d) = (max a c, min b d)
    getBound (a, b) = (ceiling $ 1000*a'/b', floor' $ 1000*(a'+1)/b') where
        a' = fromIntegral a :: Double
        b' = fromIntegral b :: Double
        floor' x = if x /= fromIntegral (floor x) then floor x else floor x - 1

showCoef :: [(Int, Int)] -> String
showCoef ls = "[" <> show l <> ", " <> show r <> "]" where
    (l, r) = calcCoef ls

main :: IO ()
main = do
    let Just x = genStatMaybe (Insight 3, Level 37) (fromRaw (36, 269, 408, 683, 1019, 1199))
    print x
    let arcList = [SomeArcanist (Arcanist :: Arcanist ThirtySeven)]
    print $ map getName arcList

    putStrLn $ showCoef [(1757, 7031)]
    putStrLn $ showCoef [(212, 922)]
    putStrLn $ showCoef [(128, 534), (145, 605), (118, 492), (134, 559), (138, 579)]
