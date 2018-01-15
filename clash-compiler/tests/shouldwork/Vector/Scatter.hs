module Scatter where

import Clash.Prelude

topEntity :: Vec 5 (Unsigned 10) -> Vec 5 (Unsigned 10)
topEntity = scatter defvec to
  where
    defvec = replicate d5 99
    to = 0 :> 4 :> 2 :> 3 :> 1 :> Nil
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator ((1 :> 2 :> 3 :> 4 :> 5 :> Nil) :> Nil)
    expectedOutput = outputVerifier   ((1 :> 5 :> 3 :> 4 :> 2 :> Nil) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
