{-# LANGUAGE BangPatterns #-}
module Strict where
import Clash.Prelude

{-
Test the strict evaluation of Unsigned values

Because Unsigned is a newtype wrapper around Integer, these `seq`s generate
casts between Unsigned and Integer which Clash needs to handle.

Also we need to be careful to not store the intermediate result as Integer,
 because Integer is stored in HDL as 64bits.
The testbench checks that we don't accidentally truncate to 64bits.
-}

type Nr = Unsigned (64+4)

big1,big2 :: Nr
big1 = 0x0ffffffffffffffff -- == maxBound :: Unsigned 64
big2 = 0xb0000000000000001

topEntity :: Nr -> Nr -> Vec 4 Nr
topEntity a b = f a b
                ++ g a b
                ++ concat (map (f a) (b:>b:>Nil))


f :: Nr -> Nr -> Vec 1 Nr
f x y = let res = x + y
        in res `seq` res :> Nil
{-# NOINLINE f #-}


g :: Nr -> Nr -> Vec 1 Nr
g x !y = x `seq` x + y :> Nil
{-# NOINLINE g #-}


testBench :: Signal System Bool
testBench = done'
  where
    testInput    = stimuliGenerator ((big1,big2) :> Nil)
    testEntity   = fmap $ map (`shiftR` 64) . uncurry topEntity
    expectOutput = outputVerifier (repeat 0xc :> Nil)
    done         = expectOutput (testEntity testInput)
    done'        = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
