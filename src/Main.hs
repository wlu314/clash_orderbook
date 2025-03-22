{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}

module Main where

import Clash.Prelude
import HashTable
import qualified Prelude as P
import Debug.Trace (traceShowId)

-- Test input: a sequence of ITCH instructions.
-- We test:
--   1. ITCH_ADD to insert an order.
--   2. ITCH_DELETE to look up and remove that order.
--   3. second ITCH_ADD for a different order.
--   4. ITCH_CANCEL to look up and remove the second order.
--   5. ITCH_OEXEC which should pass through unchanged.
--   6. Another ITCH_ADD for a new order.

testInput :: Vec 6 Inst
testInput =
  let inst0 = Inst { opcode     = ITCH_ADD
                   , valid      = True
                   , timestamp  = 0
                   , side       = Bid
                   , orderID    = 0x0000000000000123  -- lower 8 bits: 0x23 (35 decimal)
                   , price      = 100
                   , quantity   = 10
                   , newOrderID = 0 }
      inst1 = Inst { opcode     = ITCH_DELETE
                   , valid      = True
                   , timestamp  = 1
                   , side       = Bid
                   , orderID    = 0x0000000000000123  -- should return price=100, quantity=10 and replace with default
                   , price      = 0
                   , quantity   = 0
                   , newOrderID = 0 }
      inst2 = Inst { opcode     = ITCH_ADD
                   , valid      = True
                   , timestamp  = 2
                   , side       = Ask
                   , orderID    = 0x0000000000000456  -- lower 8 bits: 0x56 (86 decimal)
                   , price      = 200
                   , quantity   = 20
                   , newOrderID = 0 }
      inst3 = Inst { opcode     = ITCH_CANCEL
                   , valid      = True
                   , timestamp  = 3
                   , side       = Ask
                   , orderID    = 0x0000000000000456  -- should return price=200, quantity=20 and replace with default
                   , price      = 0
                   , quantity   = 0
                   , newOrderID = 0 }
      inst4 = Inst { opcode     = ITCH_OEXEC
                   , valid      = True
                   , timestamp  = 4
                   , side       = Ask
                   , orderID    = 0x0000000000000456
                   , price      = 300  -- this value should remain unchanged because ITCH_OEXEC is not handled.
                   , quantity   = 30
                   , newOrderID = 0 }
      inst5 = Inst { opcode     = ITCH_ADD
                   , valid      = True
                   , timestamp  = 5
                   , side       = Bid
                   , orderID    = 0x0000000000000789  -- lower 8 bits: 0x89 (137 decimal)
                   , price      = 400
                   , quantity   = 40
                   , newOrderID = 0 }
  in  inst0 :> inst1 :> inst2 :> inst3 :> inst4 :> inst5 :> Nil

-- Expected output:
-- ITCH_ADD and ITCH_OEXEC: the output is unchanged.
-- ITCH_DELETE and ITCH_CANCEL: the output is modified to include the looked-up price and quantity.
expectedOutput :: Vec 6 Inst
expectedOutput =
  let out0 = inst0
      out1 = inst1 { price = 100, quantity = 10 }  -- lookup from inst0 insertion.
      out2 = inst2
      out3 = inst3 { price = 200, quantity = 20 }  -- lookup from inst2 insertion.
      out4 = inst4  -- pass through unmodified.
      out5 = inst5
  in  out0 :> out1 :> out2 :> out3 :> out4 :> out5 :> Nil

-- Top-level test bench signal.
testBench :: Signal System Bool
testBench = done
  where
    testInputSignal = stimuliGenerator clk rst testInput
    result          = traceShowId <$> hashTable testInputSignal
    expectedSignal  = outputVerifier' clk rst expectedOutput
    done            = expectedSignal result
    clk             = tbSystemClockGen (not <$> done)
    rst             = systemResetGen

-- Main entry point for simulation.
main :: IO ()
main = print (sample testBench)
