{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module OpCode.StructureParser where

import Numeric.Natural

import qualified Data.ByteString as B

import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.Combinator

import OpCode.Type
import OpCode.Utils


satisfy :: (Stream s m OpCode) => (OpCode -> Bool) -> ParsecT s u m OpCode
satisfy f = tokenPrim (\c -> show [c])
                -- (\pos c _cs -> updatePosChar pos c)
                (\pos c _cs -> pos)
                (\c -> if f c then Just c else Nothing)

opCode :: (Stream s m OpCode) => OpCode -> ParsecT s u m OpCode
opCode opc = satisfy ((==) opc)

anyOpCode :: (Stream s m OpCode) => ParsecT s u m OpCode
anyOpCode = satisfy (const True)

pushVal :: (Stream s m OpCode) => ParsecT s u m Natural
pushVal = tokenPrim (\c -> show [c])
    -- (\pos c _cs -> updatePosChar pos c)
    (\pos c _cs -> pos)
    (\c -> if isPush c then Just (getPushVal c) else Nothing)

type ErrorAddress = Natural
data StructuredCode
    = ProtectedStoreCall (Natural, Natural)
    | UnprotectedStoreCall
    | SystemCall ErrorAddress
    | OtherOpCode OpCode
    deriving (Show, Eq)

fullStructuredParse ::[OpCode] -> Either ParseError [StructuredCode]
fullStructuredParse code = parse (fullStructureParser <* eof) "fullStructuredParse" code

-- |Parse a contract into structured blocks.
fullStructureParser :: (Stream s m OpCode) => ParsecT s u m [StructuredCode]
fullStructureParser = many (choice
    [ ProtectedStoreCall <$> (try parseLoggedAndProtectedSSTORE)
    , pure UnprotectedStoreCall <* (opCode SSTORE)
    -- , StaticSystemCall <$> parseStaticSystemCall
    , parseDynamicSystemCall
    , OtherOpCode <$> anyOpCode
    ])

parseLoggedAndProtectedSSTORE :: (Stream s m OpCode) => ParsecT s u m (Natural, Natural)
parseLoggedAndProtectedSSTORE = do
    range <- parseProtectStoreCallLeaveKey
    parseLogStoreCall
    pure range

-- parseStaticSystemCall :: (Stream s m Opcode) => ParsecT s u m StaticSystemCall
-- parseStaticSystemCall = do
--     opcode 1
--     opcode 2
--     opcode 3
--     opCode CALLER
--     opCode DUP
--     opCode OpCode.Type.EQ
--     opCode NOT
--     err_addr <- pushVal
--     opCode JUMPI
--     opcode DELEGATECALL
--     pure $ SystemCall err_addr

parseDynamicSystemCall :: (Stream s m OpCode) => ParsecT s u m StructuredCode
parseDynamicSystemCall = do
    opCode CALLER         -- CALLER         // Get Caller
    opCode DUP1           -- DUP            // Duplicate to Stack
    opCode OpCode.Type.EQ -- EQ             // Check if Caller is Kernel Instance
    opCode NOT            -- NOT            // If not..
    err_addr <- pushVal   -- PUSH erraddr   // Throw “Wrong Address” error
    opCode JUMPI          -- JUMPI          // Otherwise..
    opCode DELEGATECALL   -- DELEGATECALL   // Delegate Call to Caller
    pure $ SystemCall err_addr

parseProtectStoreCallLeaveKey :: (Stream s m OpCode) => ParsecT s u m (Natural, Natural)
parseProtectStoreCallLeaveKey = do
    ll <- pushVal -- lower limit (NOTE: this is now any PUSH size)
    opCode DUP2 -- duplicate store address for comparison
    opCode OpCode.Type.LT -- see if address is lower than the lower limit
    ul <- pushVal -- upper limit
    opCode DUP3 -- duplicate store address for comparison
    opCode OpCode.Type.GT -- see if the store address is higher than the upper limit
    opCode OR -- set top of stack to 1 if either is true
    opCode PC -- push the program counter to the stack, this is guaranteed to be an invalid jump destination
    opCode JUMPI -- jump if the address is out of bounds, the current address on the stack is guaranteed to be invliad and will throw an error
    opCode SWAP1 -- put the value on top with the key underneath
    opCode DUP2 -- put a copy of the key on top
    opCode SSTORE -- perform the store
    pure (ll, ul)

parseLogStoreCall :: (Stream s m OpCode) => ParsecT s u m ()
parseLogStoreCall = do
    -- Load the original values of our memory buffer onto the stack.
    opCode $ PUSH1 $ B.pack [0x60] -- 0x60
    opCode MLOAD
    opCode $ PUSH1 $ B.pack [0x80]  -- 0x80
    opCode MLOAD

    opCode ADDRESS
    opCode $ PUSH1 $ B.pack [0x60] -- 0x60
    opCode MSTORE

    opCode SWAP2
    opCode $ PUSH1 $ B.pack [0x80] -- 0x80
    opCode MSTORE

    topic <- pushVal
    opCode $ PUSH1 $ B.pack [0x34] -- 0x34
    opCode $ PUSH1 $ B.pack [0x6c] -- 0x6c

    opCode LOG1

    opCode $ PUSH1 $ B.pack [0x60] -- 0x60
    opCode MSTORE
    opCode $ PUSH1 $ B.pack [0x80] -- 0x80
    opCode MSTORE
    pure ()
    where
        topic = keccak256Bytes "KERNEL_SSTORE"
