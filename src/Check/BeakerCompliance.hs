-- Force strict rules for this file due to the compliance checking. Flagging a
-- warning on error on incomplete options is optional in GHC, but we want it to
-- ALWAYS be applied for this file.
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
module Check.BeakerCompliance where

import Prelude hiding (LT, EQ, GT)

import OpCode.Type

import Data.List (foldl')

findNonCompliances :: [StructuredCode] -> [NonCompliance]
findNonCompliances structures = reverse $ foldl' f [] structures
    where
        f acc structure = case compliance structure of
            Nothing -> acc
            Just nonComp -> nonComp:acc

-- |We use a whitelist, which is very verbose, but safer to maintain
compliance :: StructuredCode -> Maybe NonCompliance
compliance (SystemCall _) = Nothing
compliance (OtherOpCode STOP) = Nothing
compliance (OtherOpCode ADD) = Nothing
compliance (OtherOpCode MUL) = Nothing
compliance (OtherOpCode SUB) = Nothing
compliance (OtherOpCode DIV) = Nothing
compliance (OtherOpCode SDIV) = Nothing
compliance (OtherOpCode MOD) = Nothing
compliance (OtherOpCode SMOD) = Nothing
compliance (OtherOpCode ADDMOD) = Nothing
compliance (OtherOpCode MULMOD) = Nothing
compliance (OtherOpCode EXP) = Nothing
compliance (OtherOpCode SIGNEXTEND) = Nothing
compliance (OtherOpCode LT) = Nothing
compliance (OtherOpCode GT) = Nothing
compliance (OtherOpCode SLT) = Nothing
compliance (OtherOpCode SGT) = Nothing
compliance (OtherOpCode EQ) = Nothing
compliance (OtherOpCode ISZERO) = Nothing
compliance (OtherOpCode AND) = Nothing
compliance (OtherOpCode OR) = Nothing
compliance (OtherOpCode XOR) = Nothing
compliance (OtherOpCode NOT) = Nothing
compliance (OtherOpCode BYTE) = Nothing
compliance (OtherOpCode SHA3) = Nothing
compliance (OtherOpCode ADDRESS) = Nothing
compliance (OtherOpCode BALANCE) = Nothing
compliance (OtherOpCode ORIGIN) = Nothing
compliance (OtherOpCode CALLER) = Nothing
compliance (OtherOpCode CALLVALUE) = Nothing
compliance (OtherOpCode CALLDATALOAD) = Nothing
compliance (OtherOpCode CALLDATASIZE) = Nothing
compliance (OtherOpCode CALLDATACOPY) = Nothing
compliance (OtherOpCode CODESIZE) = Nothing
compliance (OtherOpCode CODECOPY) = Nothing
compliance (OtherOpCode GASPRICE) = Nothing
compliance (OtherOpCode EXTCODESIZE) = Nothing
compliance (OtherOpCode EXTCODECOPY) = Nothing
compliance (OtherOpCode RETURNDATASIZE) = Nothing
compliance (OtherOpCode RETURNDATACOPY) = Nothing
compliance (OtherOpCode BLOCKHASH) = Nothing
compliance (OtherOpCode COINBASE) = Nothing
compliance (OtherOpCode TIMESTAMP) = Nothing
compliance (OtherOpCode NUMBER) = Nothing
compliance (OtherOpCode DIFFICULTY) = Nothing
compliance (OtherOpCode GASLIMIT) = Nothing
compliance (OtherOpCode POP) = Nothing
compliance (OtherOpCode MLOAD) = Nothing
compliance (OtherOpCode MSTORE) = Nothing
compliance (OtherOpCode MSTORE8) = Nothing
compliance (OtherOpCode JUMP) = Nothing
compliance (OtherOpCode JUMPI) = Nothing
compliance (OtherOpCode PC) = Nothing
compliance (OtherOpCode MSIZE) = Nothing
compliance (OtherOpCode GAS) = Nothing
compliance (OtherOpCode JUMPDEST) = Nothing
compliance (OtherOpCode (PUSH1 _)) = Nothing
compliance (OtherOpCode (PUSH2 _)) = Nothing
compliance (OtherOpCode (PUSH3 _)) = Nothing
compliance (OtherOpCode (PUSH4 _)) = Nothing
compliance (OtherOpCode (PUSH5 _)) = Nothing
compliance (OtherOpCode (PUSH6 _)) = Nothing
compliance (OtherOpCode (PUSH7 _)) = Nothing
compliance (OtherOpCode (PUSH8 _)) = Nothing
compliance (OtherOpCode (PUSH9 _)) = Nothing
compliance (OtherOpCode (PUSH10 _)) = Nothing
compliance (OtherOpCode (PUSH11 _)) = Nothing
compliance (OtherOpCode (PUSH12 _)) = Nothing
compliance (OtherOpCode (PUSH13 _)) = Nothing
compliance (OtherOpCode (PUSH14 _)) = Nothing
compliance (OtherOpCode (PUSH15 _)) = Nothing
compliance (OtherOpCode (PUSH16 _)) = Nothing
compliance (OtherOpCode (PUSH17 _)) = Nothing
compliance (OtherOpCode (PUSH18 _)) = Nothing
compliance (OtherOpCode (PUSH19 _)) = Nothing
compliance (OtherOpCode (PUSH20 _)) = Nothing
compliance (OtherOpCode (PUSH21 _)) = Nothing
compliance (OtherOpCode (PUSH22 _)) = Nothing
compliance (OtherOpCode (PUSH23 _)) = Nothing
compliance (OtherOpCode (PUSH24 _)) = Nothing
compliance (OtherOpCode (PUSH25 _)) = Nothing
compliance (OtherOpCode (PUSH26 _)) = Nothing
compliance (OtherOpCode (PUSH27 _)) = Nothing
compliance (OtherOpCode (PUSH28 _)) = Nothing
compliance (OtherOpCode (PUSH29 _)) = Nothing
compliance (OtherOpCode (PUSH30 _)) = Nothing
compliance (OtherOpCode (PUSH31 _)) = Nothing
compliance (OtherOpCode (PUSH32 _)) = Nothing
compliance (OtherOpCode DUP1) = Nothing
compliance (OtherOpCode DUP2) = Nothing
compliance (OtherOpCode DUP3) = Nothing
compliance (OtherOpCode DUP4) = Nothing
compliance (OtherOpCode DUP5) = Nothing
compliance (OtherOpCode DUP6) = Nothing
compliance (OtherOpCode DUP7) = Nothing
compliance (OtherOpCode DUP8) = Nothing
compliance (OtherOpCode DUP9) = Nothing
compliance (OtherOpCode DUP10) = Nothing
compliance (OtherOpCode DUP11) = Nothing
compliance (OtherOpCode DUP12) = Nothing
compliance (OtherOpCode DUP13) = Nothing
compliance (OtherOpCode DUP14) = Nothing
compliance (OtherOpCode DUP15) = Nothing
compliance (OtherOpCode DUP16) = Nothing
compliance (OtherOpCode SWAP1) = Nothing
compliance (OtherOpCode SWAP2) = Nothing
compliance (OtherOpCode SWAP3) = Nothing
compliance (OtherOpCode SWAP4) = Nothing
compliance (OtherOpCode SWAP5) = Nothing
compliance (OtherOpCode SWAP6) = Nothing
compliance (OtherOpCode SWAP7) = Nothing
compliance (OtherOpCode SWAP8) = Nothing
compliance (OtherOpCode SWAP9) = Nothing
compliance (OtherOpCode SWAP10) = Nothing
compliance (OtherOpCode SWAP11) = Nothing
compliance (OtherOpCode SWAP12) = Nothing
compliance (OtherOpCode SWAP13) = Nothing
compliance (OtherOpCode SWAP14) = Nothing
compliance (OtherOpCode SWAP15) = Nothing
compliance (OtherOpCode SWAP16) = Nothing

compliance (OtherOpCode RETURN) = Nothing
compliance (OtherOpCode STATICCALL) = Nothing
compliance (OtherOpCode REVERT) = Nothing
compliance (OtherOpCode INVALID) = Nothing

-- The black list
compliance a@(OtherOpCode SLOAD) = Just $ NonCompliance 0 a
compliance a@(OtherOpCode SSTORE) = Just $ NonCompliance 0 a
compliance a@(OtherOpCode LOG0) = Just $ NonCompliance 0 a
compliance a@(OtherOpCode LOG1) = Just $ NonCompliance 0 a
compliance a@(OtherOpCode LOG2) = Just $ NonCompliance 0 a
compliance a@(OtherOpCode LOG3) = Just $ NonCompliance 0 a
compliance a@(OtherOpCode LOG4) = Just $ NonCompliance 0 a
compliance a@(OtherOpCode CREATE) = Just $ NonCompliance 0 a
compliance a@(OtherOpCode CALL) = Just $ NonCompliance 0 a
compliance a@(OtherOpCode CALLCODE) = Just $ NonCompliance 0 a
compliance a@(OtherOpCode DELEGATECALL) = Just $ NonCompliance 0 a
compliance a@(OtherOpCode SELFDESTRUCT) = Just $ NonCompliance 0 a

compliance a@(ProtectedStoreCall _) = Just $ NonCompliance 0 a
compliance a@(UnprotectedStoreCall) = Just $ NonCompliance 0 a
compliance a@(StoreCallLog _) = Just $ NonCompliance 0 a
