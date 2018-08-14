-- Force strict rules for this file due to the compliance checking. Flagging a
-- warning on error on incomplete options is optional in GHC, but we want it to
-- ALWAYS be applied for this file.
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Werror #-}
module Check.BeakerCompliance where

import Prelude hiding (LT, EQ, GT)

import OpCode.Type

import Data.List (foldl')

printNonCompliance :: [OpCode] -> NonCompliance -> IO ()
printNonCompliance opcodes (NonCompliance _ (StructuredCode sci scc)) = do
    putStr "Non-Compliance:"
    putStr $ " Offset: " ++ (show $ sci_startOffset sci)
    putStr $ " Size: " ++ (show $ sci_size sci)
    putStrLn $ " Element: " ++ (show scc)
    -- putStr "Non-Compliance:"1
    -- TODO: find the opcodes around it and show it

findNonCompliances :: [StructuredCode] -> [NonCompliance]
findNonCompliances structures = reverse $ foldl' f [] structures
    where
        f acc structure = case compliance structure of
            Nothing -> acc
            Just nonComp -> nonComp:acc

-- |This is the beaker compliance whitelist. A whitelist is very verbose, but
-- safer to maintain. This function looks at a single element of structued code
-- and returns a Nothing if it is compliant and a Just if it is not.
compliance :: StructuredCode -> Maybe NonCompliance
compliance (StructuredCode _ (SystemCall _)) = Nothing
compliance (StructuredCode _ (OtherOpCode STOP)) = Nothing
compliance (StructuredCode _ (OtherOpCode ADD)) = Nothing
compliance (StructuredCode _ (OtherOpCode MUL)) = Nothing
compliance (StructuredCode _ (OtherOpCode SUB)) = Nothing
compliance (StructuredCode _ (OtherOpCode DIV)) = Nothing
compliance (StructuredCode _ (OtherOpCode SDIV)) = Nothing
compliance (StructuredCode _ (OtherOpCode MOD)) = Nothing
compliance (StructuredCode _ (OtherOpCode SMOD)) = Nothing
compliance (StructuredCode _ (OtherOpCode ADDMOD)) = Nothing
compliance (StructuredCode _ (OtherOpCode MULMOD)) = Nothing
compliance (StructuredCode _ (OtherOpCode EXP)) = Nothing
compliance (StructuredCode _ (OtherOpCode SIGNEXTEND)) = Nothing
compliance (StructuredCode _ (OtherOpCode LT)) = Nothing
compliance (StructuredCode _ (OtherOpCode GT)) = Nothing
compliance (StructuredCode _ (OtherOpCode SLT)) = Nothing
compliance (StructuredCode _ (OtherOpCode SGT)) = Nothing
compliance (StructuredCode _ (OtherOpCode EQ)) = Nothing
compliance (StructuredCode _ (OtherOpCode ISZERO)) = Nothing
compliance (StructuredCode _ (OtherOpCode AND)) = Nothing
compliance (StructuredCode _ (OtherOpCode OR)) = Nothing
compliance (StructuredCode _ (OtherOpCode XOR)) = Nothing
compliance (StructuredCode _ (OtherOpCode NOT)) = Nothing
compliance (StructuredCode _ (OtherOpCode BYTE)) = Nothing
compliance (StructuredCode _ (OtherOpCode SHA3)) = Nothing
compliance (StructuredCode _ (OtherOpCode ADDRESS)) = Nothing
compliance (StructuredCode _ (OtherOpCode BALANCE)) = Nothing
compliance (StructuredCode _ (OtherOpCode ORIGIN)) = Nothing
compliance (StructuredCode _ (OtherOpCode CALLER)) = Nothing
compliance (StructuredCode _ (OtherOpCode CALLVALUE)) = Nothing
compliance (StructuredCode _ (OtherOpCode CALLDATALOAD)) = Nothing
compliance (StructuredCode _ (OtherOpCode CALLDATASIZE)) = Nothing
compliance (StructuredCode _ (OtherOpCode CALLDATACOPY)) = Nothing
compliance (StructuredCode _ (OtherOpCode CODESIZE)) = Nothing
compliance (StructuredCode _ (OtherOpCode CODECOPY)) = Nothing
compliance (StructuredCode _ (OtherOpCode GASPRICE)) = Nothing
compliance (StructuredCode _ (OtherOpCode EXTCODESIZE)) = Nothing
compliance (StructuredCode _ (OtherOpCode EXTCODECOPY)) = Nothing
compliance (StructuredCode _ (OtherOpCode RETURNDATASIZE)) = Nothing
compliance (StructuredCode _ (OtherOpCode RETURNDATACOPY)) = Nothing
compliance (StructuredCode _ (OtherOpCode BLOCKHASH)) = Nothing
compliance (StructuredCode _ (OtherOpCode COINBASE)) = Nothing
compliance (StructuredCode _ (OtherOpCode TIMESTAMP)) = Nothing
compliance (StructuredCode _ (OtherOpCode NUMBER)) = Nothing
compliance (StructuredCode _ (OtherOpCode DIFFICULTY)) = Nothing
compliance (StructuredCode _ (OtherOpCode GASLIMIT)) = Nothing
compliance (StructuredCode _ (OtherOpCode POP)) = Nothing
compliance (StructuredCode _ (OtherOpCode MLOAD)) = Nothing
compliance (StructuredCode _ (OtherOpCode MSTORE)) = Nothing
compliance (StructuredCode _ (OtherOpCode MSTORE8)) = Nothing
compliance (StructuredCode _ (OtherOpCode JUMP)) = Nothing
compliance (StructuredCode _ (OtherOpCode JUMPI)) = Nothing
compliance (StructuredCode _ (OtherOpCode PC)) = Nothing
compliance (StructuredCode _ (OtherOpCode MSIZE)) = Nothing
compliance (StructuredCode _ (OtherOpCode GAS)) = Nothing
compliance (StructuredCode _ (OtherOpCode JUMPDEST)) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH1 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH2 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH3 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH4 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH5 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH6 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH7 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH8 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH9 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH10 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH11 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH12 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH13 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH14 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH15 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH16 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH17 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH18 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH19 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH20 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH21 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH22 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH23 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH24 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH25 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH26 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH27 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH28 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH29 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH30 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH31 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode (PUSH32 _))) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP1)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP2)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP3)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP4)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP5)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP6)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP7)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP8)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP9)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP10)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP11)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP12)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP13)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP14)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP15)) = Nothing
compliance (StructuredCode _ (OtherOpCode DUP16)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP1)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP2)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP3)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP4)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP5)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP6)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP7)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP8)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP9)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP10)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP11)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP12)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP13)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP14)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP15)) = Nothing
compliance (StructuredCode _ (OtherOpCode SWAP16)) = Nothing

compliance (StructuredCode _ (OtherOpCode RETURN)) = Nothing
compliance (StructuredCode _ (OtherOpCode STATICCALL)) = Nothing
compliance (StructuredCode _ (OtherOpCode REVERT)) = Nothing
compliance (StructuredCode _ (OtherOpCode INVALID)) = Nothing

-- The black list
compliance a@(StructuredCode _ (OtherOpCode SLOAD)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (OtherOpCode SSTORE)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (OtherOpCode LOG0)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (OtherOpCode LOG1)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (OtherOpCode LOG2)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (OtherOpCode LOG3)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (OtherOpCode LOG4)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (OtherOpCode CREATE)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (OtherOpCode CALL)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (OtherOpCode CALLCODE)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (OtherOpCode DELEGATECALL)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (OtherOpCode SELFDESTRUCT)) = Just $ NonCompliance 0 a

compliance a@(StructuredCode _ (ProtectedStoreCall _)) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ UnprotectedStoreCall) = Just $ NonCompliance 0 a
compliance a@(StructuredCode _ (StoreCallLog _)) = Just $ NonCompliance 0 a
