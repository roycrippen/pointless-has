{-# LANGUAGE MagicHash #-}
module PopCount where

import Clash.Prelude
import GHC.Word
import Data.Bits

topEntity :: Word -> Int
topEntity = popCount
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator $(listToVecTH [1::Word,3,8,50,0])
    expectedOutput = outputVerifier   $(listToVecTH ([1,2,1,3,0]::[Int]))
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
