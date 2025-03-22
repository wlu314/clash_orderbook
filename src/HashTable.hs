-- This file defines the hash table's interface by:
-- Identifying the input signals (ITCH message - orderID) and outputs (updated book details)
-- Define the hash table interface as a function that accepts keys and the operations: insertion, lookup, deletion, etc.
-- Integrate Hash Function
-- Control logic Handles specific ITCH instruction to remove orderID if message is DELETE
-- Takes 8-bytes as key

-- Incoming ITCH instruciton is passed thporuhg the ordermap strage
-- ITCH_ADD: price and quantity is stored using key orderID
-- ITCH_DELETE/ITCH_CANCEL: order details are looked up and cleared

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module HashTable where

import Clash.Prelude
import GHC.Generics (Generic)
import Clash.Class.BitPack (BitPack) 

-- Define widths (in bits from ITCH protocol)
type OrderIdWidth = 64
type PriceWidth = 32
type QuantityWidth = 32

-- ITCH opcodes
data Opcode = ITCH_ADD | ITCH_OEXEC | ITCH_PEXEC | ITCH_CANCEL | ITCH_DELETE | ITCH_REPLACE
  deriving (Show, Generic, Eq, BitPack)
  deriving anyclass (NFDataX)

data Side = Bid | Ask
  deriving (Show, Generic, Eq, BitPack)
  deriving anyclass (NFDataX)

data Inst = Inst
  {   opcode     :: Opcode
    , valid      :: Bool
    , timestamp  :: BitVector 64
    , side       :: Side
    , orderID    :: BitVector OrderIdWidth
    , price      :: BitVector PriceWidth
    , quantity   :: BitVector QuantityWidth
    , newOrderID :: BitVector OrderIdWidth 
  }
  deriving (Show, Generic, BitPack)
  deriving anyclass (NFDataX)

data OrderEntry = OrderEntry
  { oeValid     :: Bool
  , oePrice     :: BitVector PriceWidth
  , oeQuantity  :: BitVector QuantityWidth
  }
  deriving stock    (Show, Generic, Eq, BitPack)
  deriving anyclass (NFDataX)

-- Default order entry
defaultOrderEntry :: OrderEntry
defaultOrderEntry = OrderEntry False 0 0 -- Valid, Price, Quantity

-- Size of Hash Table: power of 2 size 
type MapSize = 256

-- hashTable Imeplements hash table (order map) stage
-- ITCH_ADD: Order details are added to the table
-- ITCH_DELETE/ITCH_CANCEL: The stored entry is read out and then cleared (written over by 'defaultOrderEntry')
hashTable
  :: HiddenClockResetEnable dom
  => Signal dom Inst  -- Input: ITCH instruction stream
  -> Signal dom Inst  -- Output: ITCH instruction stream
hashTable instIn = instOut
  where 
    -- Use the lower 8 bits of the orderID as address
    addr :: Signal dom (Unsigned 8)
    addr = fmap (\inst -> unpack (slice d7 d0 (orderID inst))) instIn

    -- Generate write-enable signal based on the opcode
    -- write for ITCH_ADD/DELETE/CANCEL
    writeEn :: Signal dom Bool
    writeEn = fmap (\inst -> case opcode inst of
                                ITCH_ADD    -> True
                                ITCH_DELETE -> True
                                ITCH_CANCEL -> True
                                _           -> False) instIn
    
    -- Data written in Hash Table
    -- ADD: Store the new order
    -- DELETE/CANCEL: Clear the entry
    writeData :: Signal dom OrderEntry
    writeData = fmap (\inst -> case opcode inst of
                                  ITCH_ADD    -> OrderEntry True (price inst) (quantity inst)
                                  ITCH_DELETE -> defaultOrderEntry
                                  ITCH_CANCEL -> defaultOrderEntry
                                  _           -> defaultOrderEntry) instIn

    -- Address, data, and write enable into the write port
    writePort :: Signal dom (Maybe (Unsigned 8, OrderEntry))
    writePort = liftA2 (\we (a, d) -> if we then Just (a, d) else Nothing)
                        writeEn (bundle (addr, writeData))

    -- initialize the hash table with default entry
    initialTable :: Vec MapSize OrderEntry
    initialTable = replicate d256 defaultOrderEntry
    
    -- create the block RAM (synchronous read) using blockRamPow2
    tableOut :: Signal dom OrderEntry
    tableOut = blockRamPow2 initialTable addr writePort

    -- DELETE and CANCEL instructions, update the instruction with looked-up order details
    instOut = liftA2 (\inst oe ->
                        case opcode inst of
                          ITCH_DELETE -> inst { price = oePrice oe, quantity = oeQuantity oe }
                          ITCH_CANCEL -> inst { price = oePrice oe, quantity = oeQuantity oe }
                          _           -> inst)
                     instIn tableOut
