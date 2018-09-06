{-|
Module      : OSMain
Description : Operatin System Type tests
Copyright   :
License     :
Maintainer  : jake@daolab.io
Stability   :
Portability :

This tests behaviour of certain chunks of code the will go into building
beakeros. For example it tests the behaviour around DELEGATECALL and CALLCODE
how they will behave in the context of an operating system or kernel.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Exception

import Data.Attoparsec.ByteString
import qualified Text.Parsec.Prim as Parsec
import qualified Text.Parsec.Combinator as Parsec
import Data.ByteString (pack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import Data.ByteString.Base16 as B16
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (mempty, (<>))
import qualified Data.Set as S
import Data.Text.Encoding
import Data.Either

import Numeric.Natural

import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Check.Stores
import OpCode.Exporter
import OpCode.Parser
import OpCode.StructureParser
import OpCode.Type
import Process
import OpCode.Utils
import CompileSolidity
import Models.HandWritten

import Network.Ethereum.Web3 hiding (runWeb3)
import Network.Ethereum.Web3.Web3
import Network.Ethereum.Web3.JsonAbi as JsonAbi
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Eth as Eth
import qualified Network.Ethereum.Web3.Address as Address

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Temp

-- Import code for parsing opcodes and all low level data handling.
import Tests.HandleOpCodes
-- Import code that can analyse the opcodes and perform tests and checks
import Tests.Analyse
-- Import code that can perform transformations on the opcodes
import Tests.Transform
-- Import the base utilities that are shared with all test code
import Tests.Utils

main = do
    putStrLn "Running tests..."
    defaultMain tests

mainWithOpts = do

    -- Test options can also be specified in the code. The TestOptions
    -- type is an instance of the Monoid type class, so the easiest way
    -- to get an empty set of options is with `mempty`.
    let empty_test_opts = mempty :: TestOptions

    -- We update the empty TestOptions with our desired values.
    let my_test_opts = empty_test_opts
            { topt_maximum_generated_tests = Just 1000
            , topt_timeout = Just $ Just (4000::Int)
            }

    -- Now we create an empty RunnerOptions in the same way, and add
    -- our TestOptions to it.
    let empty_runner_opts = mempty :: RunnerOptions
    let my_runner_opts = empty_runner_opts
            { ropt_test_options = Just my_test_opts
            }

    defaultMainWithOpts tests my_runner_opts

tests = -- [ testGroup "Single Test" $ hUnitTestToTests storeAndGetOnChainProtected ]
    [ testGroup "Initial OS Tests" $ (hUnitTestToTests osTests)
    , testGroup "Jumps" $ hUnitTestToTests jumpTests
    ]

osTests = TestList
    [ trivialOnChain
    , trivialOnChainAutoDeploy
    , testCaller
    , testKernelCallerCALLCODE
    , testKernelCallerDELEGATECALL
    ]

-- |Deploy a trivial example of a contract on chain and check that the code on
-- chain is what we wanted deployed. In this case the deployment code is written
-- manually.
--
-- This trivial contract doesn't actually do anything and is just randm opcodes.
trivialOnChain = TestLabel "Trivial on chain" $ TestCase $ do
        let bytecode =
                -- Start deployment code
                [ PUSH1 (pack [0x10])
                , DUP1
                , PUSH1 (pack [0x0f])
                , PUSH1 (pack [0x00])
                , CODECOPY
                , PUSH1 (pack [0x00])
                , RETURN
                , STOP
                -- End deployment code
                -- Start of contract

                -- Call data starting from position 0
                , PUSH1 (pack [0x00])
                , CALLDATALOAD

                -- Load storage at the address found in the call data
                , SLOAD

                -- Negate every bit of this value
                , NOT

                -- If this new value is non-zero, jump to position 9
                , PUSH1 (pack [0x09])
                , JUMPI
                , STOP
                , PUSH1 (pack [0x20])
                , CALLDATALOAD
                , PUSH1 (pack [0x00])
                , CALLDATALOAD
                , SSTORE
                -- End of contract
                ]

        let bsEncoded = B16.encode $ B.concat $ map toByteString bytecode
        (Right sender) <- runWeb3 $ do
            accs <- accounts
            case accs of
                    [] -> error "No accounts available"
                    (a:_) -> pure a
        (res, tx) <- deployContract sender bsEncoded
        newContractAddress <- getContractAddress tx

        (Right code) <- runWeb3 $ getCode newContractAddress Latest
        actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code
        pure ()

-- |Deploy a trivial example of a contract on chain and check that the code on
-- chain is what we wanted deployed. This uses the function @makeDeployable@ to
-- convert normal contract code into something that can be deployed (with no
-- special constructor code).
trivialOnChainAutoDeploy = TestLabel "Trivial on chain auto-deploy" $ TestCase $ do
    let deployable = makeDeployable kernelCode

    let bsEncoded = B16.encode $ B.concat $ map toByteString deployable
    (Right sender) <- runWeb3 $ do
        accs <- accounts
        case accs of
                [] -> error "No accounts available"
                (a:_) -> pure a
    (res, tx) <- deployContract sender bsEncoded
    newContractAddress <- getContractAddress tx

    (Right code) <- runWeb3 $ getCode newContractAddress Latest
    actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code
    assertEqual "Actual run code should be the same as initial code" kernelCode actualRunCode
    pure ()

-- |This deploys and runs the @returnCallerCode@ contract, and tests that it
-- correctly returns our address.
testCaller = TestLabel "Test Caller" $ TestCase $ do
    let deployable = makeDeployable returnCallerCode

    let bsEncoded = B16.encode $ B.concat $ map toByteString deployable
    (Right sender) <- runWeb3 $ do
        accs <- accounts
        case accs of
                [] -> error "No accounts available"
                (a:_) -> pure a
    (res, tx) <- deployContract sender bsEncoded
    newContractAddress <- getContractAddress tx

    (Right code) <- runWeb3 $ getCode newContractAddress Latest
    actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code
    (Right result) <- retrieveCaller newContractAddress
    (Right sender) <-  runWeb3 $ do
        accs <- accounts
        let sender = case accs of
                [] -> error "No accounts available"
                (a:_) -> a
        pure sender
    assertEqual "The caller should equal the sender" ("0x" <> Address.toText sender) result
    pure ()
    where
        retrieveCaller newContractAddress = runWeb3 $ do
            accs <- accounts
            let sender = case accs of
                    [] -> error "No accounts available"
                    (a:_) -> a
            let details = Call {
                    callFrom = Just sender,
                    callTo = Just newContractAddress,
                    callGas = Nothing,
                    callGasPrice = Nothing,
                    callValue = Nothing,
                    callData = Nothing
                }
            theCall <- Eth.call details Latest -- TODO: switch back to using this for the result
            pure (theCall)

-- |This:
--
-- 1. Deploys @returnCallerCode@ contract.
-- 2. Deploys a "customKernel" contract.
-- 3. Sends a transaction to the kernel contract asking it to call the
--    @returnCallerCode@ contract (using DELEGATECALL).
-- 4. The kernel then returns the return value from @returnCallerCode@.
-- 5. Checks that the address @returnCallerCode@ gave is the address of the
--    original sender, as we are using DELEGATECALL.
testKernelCallerDELEGATECALL = TestLabel "Test Kernel Caller (DELEGATECALL)" $ TestCase $ do
    let deployable = makeDeployable returnCallerCode

    -- Get the account we will be using
    (Right sender) <- runWeb3 $ do
        accs <- accounts
        case accs of
            [] -> error "No accounts available"
            (a:_) -> pure a
    -- Encode the @returnCallerCode@ contract
    let bsEncoded = B16.encode $ B.concat $ map toByteString deployable
    -- Deploy the @returnCallerCode@ contract
    (res, tx) <- deployContract sender bsEncoded
    -- Get the address of this deployed @returnCallerCode@ contract
    newContractAddress <- getContractAddress tx

    -- Create a kernel that calls this @returnCallerCode@ contract
    let newKernelCode = customKernelCodeDELEGATECALL newContractAddress
        deployableKernelCode = makeDeployable newKernelCode
    -- Encode the kernel
    let bsEncoded = B16.encode $ B.concat $ map toByteString deployableKernelCode
    -- Deploy the kernel
    (_, txK) <- deployContract sender bsEncoded
    -- Retrieve the kernel address
    kernelAddress <- getContractAddress txK

    (Right code) <- runWeb3 $ getCode kernelAddress Latest
    actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code

    -- Call the kernel (which in turn calls the contract)
    (Right result) <- retrieveCaller kernelAddress

    assertEqual "The caller should equal the sender" ("0x" <> Address.toText sender) result
    pure ()
    where
        retrieveCaller newContractAddress = runWeb3 $ do
            accs <- accounts
            let sender = case accs of
                    [] -> error "No accounts available"
                    (a:_) -> a
            let details = Call {
                    callFrom = Just sender,
                    callTo = Just newContractAddress,
                    callGas = Nothing,
                    callGasPrice = Nothing,
                    callValue = Nothing,
                    callData = Nothing
                }
            theCall <- Eth.call details Latest -- TODO: switch back to using this for the result
            pure (theCall)

-- |This:
--
-- 1. Deploys @returnCallerCode@ contract.
-- 2. Deploys a "customKernel" contract.
-- 3. Sends a transaction to the kernel contract asking it to call the
--    @returnCallerCode@ contract (using CALLCODE).
-- 4. The kernel then returns the return value from @returnCallerCode@.
-- 5. Checks that the address @returnCallerCode@ gave is the address of the
--    kernel, as we are using CALLCODE.
testKernelCallerCALLCODE = TestLabel "Test Kernel Caller (CALLCODE)" $ TestCase $ do
    let deployable = makeDeployable returnCallerCode

    -- Get the account we will be using
    (Right sender) <- runWeb3 $ do
        accs <- accounts
        case accs of
            [] -> error "No accounts available"
            (a:_) -> pure a
    -- Encode the @returnCallerCode@ contract
    let bsEncoded = B16.encode $ B.concat $ map toByteString deployable
    -- Deploy the @returnCallerCode@ contract
    (res, tx) <- deployContract sender bsEncoded
    -- Get the address of this deployed @returnCallerCode@ contract
    newContractAddress <- getContractAddress tx

    -- Create a kernel that calls this @returnCallerCode@ contract
    let newKernelCode = customKernelCodeCALLCODE newContractAddress
        deployableKernelCode = makeDeployable newKernelCode
    -- Encode the kernel
    let bsEncoded = B16.encode $ B.concat $ map toByteString deployableKernelCode
    -- Deploy the kernel
    (_, txK) <- deployContract sender bsEncoded
    -- Retrieve the kernel address
    kernelAddress <- getContractAddress txK

    (Right code) <- runWeb3 $ getCode kernelAddress Latest
    actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code

    -- Call the kernel (which in turn calls the contract)
    (Right result) <- retrieveCaller kernelAddress
    assertEqual "The caller should equal the kernel" ("0x" <> Address.toText kernelAddress) result
    pure ()
    where
        retrieveCaller newContractAddress = runWeb3 $ do
            accs <- accounts
            let sender = case accs of
                    [] -> error "No accounts available"
                    (a:_) -> a
            let details = Call {
                    callFrom = Just sender,
                    callTo = Just newContractAddress,
                    callGas = Nothing,
                    callGasPrice = Nothing,
                    callValue = Nothing,
                    callData = Nothing
                }
            theCall <- Eth.call details Latest -- TODO: switch back to using this for the result
            pure (theCall)

-- |Take bytecode in a deployed format and make it deployable with no
-- constructor.
makeDeployable :: [OpCode] -> [OpCode]
makeDeployable code = deployHeader ++ code
    where
        memStart = 0x10
        codeStart = sum $ map nBytes deployHeader
        codeLength = sum $ map nBytes code
        deployHeader =
            [ PUSH1 (pack [fromIntegral codeLength]) -- TODO: consider longer
            , DUP1
            , PUSH1 (pack [fromIntegral codeStart])
            , PUSH1 (pack [0x00])
            , CODECOPY
            , PUSH1 (pack [0x00])
            , RETURN
            , STOP
            ]

-- |Non-sensical random opcodes. (I think).
kernelCode :: [OpCode]
kernelCode =
    [ SLOAD
    , NOT
    , PUSH1 (pack [0x09])
    , JUMPI
    , STOP
    , PUSH1 (pack [0x20])
    , CALLDATALOAD
    , PUSH1 (pack [0x00])
    , CALLDATALOAD
    , SSTORE
    ]
    -- delegatecall(g, a, in, insize, out, outsize)

-- |This is a simple kernel which calls the address given (at deployment). It
-- ignores any calldata. It is fixed to only receive 20 bytes (such as an
-- address) from its callee, and return 20 bytes. It uses DELEGATECALL.
customKernelCodeDELEGATECALL :: Address -> [OpCode]
customKernelCodeDELEGATECALL address =
    [ PUSH1 (pack [0x14]) -- outsize
    , PUSH1 (pack [0x00]) -- out
    , PUSH1 (pack [0x00]) -- insize
    , PUSH1 (pack [0x00]) -- in
    , PUSH20 addressBytes -- address
    , PUSH2 (pack [0xff,0xff]) -- gas
    , DELEGATECALL
    , PUSH1 (pack [0x14]) -- outsize
    , PUSH1 (pack [0x00]) -- out
    , RETURN
    ]
    where
        (addressBytes,_) = B16.decode $ encodeUtf8 $ Address.toText $ address

-- |This is a simple kernel which calls the address given (at deployment). It
-- ignores any calldata. It is fixed to only receive 20 bytes (such as an
-- address) from its callee, and return 20 bytes. It uses CALLCODE.
customKernelCodeCALLCODE :: Address -> [OpCode]
customKernelCodeCALLCODE address =
    [ PUSH1 (pack [0x14]) -- outsize
    , PUSH1 (pack [0x00]) -- out
    , PUSH1 (pack [0x00]) -- insize
    , PUSH1 (pack [0x00]) -- in
    , PUSH1 (pack [0x00]) -- send value
    , PUSH20 addressBytes -- address
    , PUSH2 (pack [0xff,0xff]) -- gas
    , CALLCODE
    -- TODO: check return result
    -- Start a quick hack to test returning
    -- put the CALLCODE error code into the first byte
    -- , PUSH1 (pack [0x00])
    -- , MSTORE8
    -- End a quick hack to test returning
    , PUSH1 (pack [0x14]) -- outsize
    , PUSH1 (pack [0x00]) -- out
    , RETURN
    ]
    where
        (addressBytes,_) = B16.decode $ encodeUtf8 $ Address.toText $ address


-- |This is a contract which simply returns the value of its caller.
returnCallerCode :: [OpCode]
returnCallerCode =
    [ CALLER
    , PUSH1 (pack [0x00])
    , MSTORE
    -- outsize, 20 (0x14) bytes, an address is 20 bytes
    , PUSH1 (pack [0x14])
    -- out, start 12 (0xc) bytes in as the as the address is in the last 20
    -- bytes (32-20=12)
    , PUSH1 (pack [0x0c])
    , RETURN
    ]

-- |This is a test to demonstrate that you can't jump into PUSH data
jumpTests = TestList
    [ TestLabel "Compliant Jump" $ TestCase $ do
        let bytecode =
                [ PUSH1 (pack [7])
                , JUMP
                , STOP
                , PUSH1 (pack [0x5b])
                , STOP
                , JUMPDEST
                , PUSH1 (pack [0xff])
                , PUSH1 (pack [0x00])
                , MSTORE
                , PUSH1 (pack [0x20])
                , PUSH1 (pack [0x00])
                , RETURN
                ]

        let bsEncoded = B16.encode $ B.concat $ map toByteString $ makeDeployable bytecode
        (Right sender) <- runWeb3 $ do
            accs <- accounts
            case accs of
                    [] -> error "No accounts available"
                    (a:_) -> pure a
        (res, tx) <- deployContract sender bsEncoded
        newContractAddress <- getContractAddress tx

        (Right code) <- runWeb3 $ getCode newContractAddress Latest
        actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code

        res <- runWeb3 $ do
            accs <- accounts
            let sender = case accs of
                    [] -> error "No accounts available"
                    (a:_) -> a
            let details = Call {
                    callFrom = Just sender,
                    callTo = Just newContractAddress,
                    callGas = Nothing,
                    callGasPrice = Nothing,
                    callValue = Nothing,
                    callData = Nothing
                }
            theCall <- Eth.call details Latest
            pure (theCall)
        assertEqual "Code should run and return the correct value" res (Right "0x00000000000000000000000000000000000000000000000000000000000000ff")
    , TestLabel "Non-Compliant Jump" $ TestCase $ do
        let bytecode =
                [ PUSH1 (pack [5])
                , JUMP
                , STOP
                , PUSH1 (pack [0x5b])
                , STOP
                , JUMPDEST
                , PUSH1 (pack [0xff])
                , PUSH1 (pack [0x00])
                , MSTORE
                , PUSH1 (pack [0x20])
                , PUSH1 (pack [0x00])
                , RETURN
                ]

        let bsEncoded = B16.encode $ B.concat $ map toByteString $ makeDeployable bytecode
        (Right sender) <- runWeb3 $ do
            accs <- accounts
            case accs of
                    [] -> error "No accounts available"
                    (a:_) -> pure a
        (res, tx) <- deployContract sender bsEncoded
        newContractAddress <- getContractAddress tx

        (Right code) <- runWeb3 $ getCode newContractAddress Latest
        actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code

        res <- runWeb3 $ do
            accs <- accounts
            let sender = case accs of
                    [] -> error "No accounts available"
                    (a:_) -> a
            let details = Call {
                    callFrom = Just sender,
                    callTo = Just newContractAddress,
                    callGas = Nothing,
                    callGasPrice = Nothing,
                    callValue = Nothing,
                    callData = Nothing
                }
            theCall <- Eth.call details Latest
            pure (theCall)
        case res of
            Right x -> error $ show x
            Left (JsonRpcFail (RpcError {})) -> pure ()
            _ -> error ""
    ]
