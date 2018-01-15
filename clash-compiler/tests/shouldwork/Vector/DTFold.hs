{-# LANGUAGE UndecidableInstances #-}
module DTFold where

import Clash.Prelude
import Data.Singletons.Prelude
import Data.Proxy

data IIndex (f :: TyFun Nat *) :: *
type instance Apply IIndex l = Index ((2^l)+1)

populationCount :: (KnownNat k, KnownNat (2^k))
                => BitVector (2^k) -> Index ((2^k)+1)
populationCount bv = dtfold (Proxy :: Proxy IIndex)
                            fromIntegral
                            (\_ x y -> plus x y)
                            (bv2v bv)

topEntity :: BitVector 16 -> Index 17
topEntity = populationCount
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator $(listToVecTH ([0..20]::[BitVector 16]))
    expectedOutput = outputVerifier   $(listToVecTH ([0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2]::[Index 17]))
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
