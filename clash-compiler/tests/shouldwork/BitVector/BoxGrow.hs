module BoxGrow where

import Clash.Prelude

ys >:> xss = zipWith (:>) ys xss
xss <:< ys = zipWith (:<) xss ys

box0 :: Vec 5 (Vec 6 Bit) -> Vec 7 (Vec 8 Bit)
box0 grid =     (zeroesM :> ((zeroesN >:> grid) <:< zeroesN)) :< zeroesM
        where
          zeroesN = replicate d5 0
          zeroesM = replicate d8 0

topEntity = box0
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure (repeat (repeat 1))
    expectedOutput = outputVerifier ( ((0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil) :>
                                      (0 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil) :>
                                      (0 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil) :>
                                      (0 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil) :>
                                      (0 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil) :>
                                      (0 :> 1 :> 1 :> 1 :> 1 :> 1 :> 1 :> 0 :> Nil) :>
                                      (0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil) :> Nil) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
