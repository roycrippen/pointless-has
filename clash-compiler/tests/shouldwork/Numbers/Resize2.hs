module Resize2 where

import Clash.Prelude

topEntity :: Signed 4 -> Signed 5
topEntity = resize
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator $(listToVecTH ([minBound .. maxBound]::[Signed 4]))
    expectedOutput = outputVerifier $(listToVecTH ([-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7]::[Signed 5]))
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
