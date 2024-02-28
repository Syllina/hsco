module Main (main) where

import System.Random
import System.Random.Stateful

import Data.List (minimum, maximum)

import TextShow

data Dice = Dice Int -- sides

sides :: Dice -> Int
sides (Dice n) = n

rollM :: StatefulGen g m => Dice -> g -> m Int
rollM (Dice sides) = uniformRM (1, sides)

data DiceModifier = Normal | Advantage | Disadvantage
instance TextShow DiceModifier where
    showb Normal = fromText ""
    showb Advantage = fromText "kh"
    showb Disadvantage = fromText "kl"
-- [n]d<f>[kh/kl] | n
data DiceExprTerm = DiceTerm Int Int DiceModifier | ConstTerm Int
instance TextShow DiceExprTerm where
    showb (DiceTerm n sides diceMod) =
        -- if n/=1 then showb n else fromText "" <>
        showb n <>
        fromText "d" <>
        showb sides <>
        showb diceMod
    showb (ConstTerm n) = showb n

data DiceExpr = Term DiceExprTerm | Plus DiceExpr DiceExpr
instance TextShow DiceExpr where
    showb (Term term) = showb term
    showb (Plus a b) = showb a <> fromText " + " <> showb b

rollTermM :: StatefulGen g m => DiceExprTerm -> g -> m Int
rollTermM (DiceTerm n sides diceMod) = fmap sum . replicateM n . rollSingleM diceMod where
    rollSingleM Normal = rollM (Dice sides)
    rollSingleM Advantage = fmap minimum . replicateM 2 . rollSingleM Normal
    rollSingleM Disadvantage = fmap maximum . replicateM 2 . rollSingleM Normal
rollTermM (ConstTerm n) = pure . const n

rollExprM :: StatefulGen g m => DiceExpr -> g -> m Int
rollExprM (Term term) = rollTermM term
rollExprM (Plus a b) = \g -> liftA2 (+) (rollExprM a g) (rollExprM b g)

expr :: DiceExpr
expr = Plus (Term $ DiceTerm 1 20 Advantage) (Plus (Term $ DiceTerm 1 12 Disadvantage) (Term $ ConstTerm 3))

main :: IO ()
main = do
    let result = runStateGen_ (mkStdGen 42) (rollExprM expr)
    putTextLn $ showt expr <> " = " <> showt result
