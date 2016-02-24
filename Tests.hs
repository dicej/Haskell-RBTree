{-# LANGUAGE FlexibleInstances #-}
import Data.Tree.RBTree
import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Random
import System.Exit
import System.Random.TF
import Control.Monad
import Text.Printf
import Debug.Trace

type Version = Int
type Value = Int

data Cell = Cell Version Value deriving (Show)

instance Eq Cell where
  a == b = compare a b == EQ

instance Ord Cell where
  compare = compareCells

updateCellVersion version (Cell _ value) = Cell version value

compareCells (Cell _ a) (Cell _ b) = compare a b

cellVersionsEqual (Cell a _) (Cell b _) = a == b

instance Arbitrary Cell where
  arbitrary = Cell 0 <$> arbitrary
  shrink (Cell version value) = Cell version <$> shrink value

instance Arbitrary (RBTree Cell) where
  arbitrary = fromList compareCells <$> arbitrary
  shrink = map (fromList compareCells) . shrink . toList

check description prop = do
  printf "%-25s: " description

  result <- quickCheckWithResult
    stdArgs {- replay = Just (QCGen $ seedTFGen (0, 0, 0, 0), 0) -} prop

  unless (isSuccess result) $ do
    putStrLn $ "Use " ++ show (usedSeed result) ++ " as the initial seed"
    putStrLn $ "Use " ++ show (usedSize result) ++ " as the initial size"
    exitFailure

diffV = diffVersioned cellVersionsEqual compareCells

diffN = diff compareCells

main = do
  check "black depth" $ \tree -> let _ = tree :: RBTree Cell in
    case vD tree of
      Nothing -> False
      _ -> True

  check "no red/red" (vR :: RBTree Cell -> Bool)

  check "diff added then deleted" $ \original added deleted ->
    let base = unionVersioned (updateCellVersion 1) compareCells original Leaf

        derived =
          subtractVersioned (updateCellVersion 2) compareCells deleted
          (unionVersioned (updateCellVersion 2) compareCells added base) in

    diffV base derived == diffN base derived

  check "diff deleted then added" $ \original deleted added ->
    let base = unionVersioned (updateCellVersion 1) compareCells original Leaf

        derived =
          unionVersioned (updateCellVersion 2) compareCells added
          (subtractVersioned (updateCellVersion 2) compareCells deleted base) in

    diffV base derived == diffN base derived
