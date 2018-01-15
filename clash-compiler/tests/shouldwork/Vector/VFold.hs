module VFold where

import Clash.Prelude

csSort = vfold (const csRow)
  where
    cs a b     = if a > b then (a,b) else (b,a)
    csRow y xs = let (y',xs') = mapAccumL cs y xs in xs' :< y'

topEntity :: Vec 4 Int -> Vec 4 Int
topEntity = csSort
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (7 :> 3 :> 9 :> 1 :> Nil)
    expectedOutput = outputVerifier ((1:>3:>7:>9:>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
