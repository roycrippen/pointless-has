module Mixer where

import Clash.Prelude

k               = 0.6
piFHalf         = 1.5707963267948966 :: SFixed 3 8

cordic angle
    | z < 0     =  k
    | otherwise = -k
    where
        z       | angle < 0 = piFHalf + angle
                | otherwise = (-piFHalf)+ angle


topEntity :: SFixed 3 8 -> SFixed 3 8
topEntity = cordic
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator (0.7853981633974483 :> Nil)
    expectedOutput = outputVerifier   (0.59765625 :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
