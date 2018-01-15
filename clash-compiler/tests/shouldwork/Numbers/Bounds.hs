module Bounds where
import Clash.Prelude


expected
    =     0 :>  42
    :> -256 :> 255
    :>    0 :> 511
    :>  -64 :>  63
    :>    0 :> 127
    :> (Nil::Vec 0 Integer)

actual
    =  toInteger (minBound :: Index 43)             :> toInteger (maxBound :: Index 43)
    :> toInteger (minBound :: Signed 9)             :> toInteger (maxBound :: Signed 9)
    :> toInteger (minBound :: Unsigned 9)           :> toInteger (maxBound :: Unsigned 9)
    :> toInteger (unFixed (minBound :: SFixed 4 3)) :> toInteger (unFixed (maxBound :: SFixed 4 3))
    :> toInteger (unFixed (minBound :: UFixed 3 4)) :> toInteger (unFixed (maxBound :: UFixed 3 4))
    :> Nil

topEntity :: SystemClockReset => Signal System () -> Signal System Integer
topEntity = mealy loop actual
{-# NOINLINE topEntity #-}

loop :: Vec (n+2) a -> () -> (Vec (n+2) a, a)
--loop (x:>xs) _ = (xs :< last xs, x)
loop xs _ = (xs <<+ last xs, head xs)

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = pure ()
    expectedOutput = outputVerifier expected
    done           = expectedOutput (topEntity testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
