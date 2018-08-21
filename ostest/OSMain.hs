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
    ]

osTests = TestList
    [ trivialOnChain
    , trivialOnChainAutoDeploy
    , testCaller
    , testKernelCaller
    ]

trivialOnChain = TestLabel "Trivial on chain" $ TestCase $ do
        let bytecode =
                [ PUSH1 (pack [0x10])
                , DUP1
                , PUSH1 (pack [0x0f])
                , PUSH1 (pack [0x00])
                , CODECOPY
                , PUSH1 (pack [0x00])
                , RETURN
                , STOP
                , PUSH1 (pack [0x00])
                , CALLDATALOAD
                , SLOAD
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
        -- mapM_ print actualRunCode
        pure ()

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
    -- assertEqual "Actual run code should be the same as initial code" kernelCode actualRunCode
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

testKernelCaller = TestLabel "Test Kernel Caller" $ TestCase $ do
    let deployable = makeDeployable returnCallerCode
    let deployableKernel = makeDeployable kernelCode

    let bsEncoded = B16.encode $ B.concat $ map toByteString deployable
    (Right sender) <- runWeb3 $ do
        accs <- accounts
        case accs of
                [] -> error "No accounts available"
                (a:_) -> pure a
    (res, tx) <- deployContract sender bsEncoded
    newContractAddress <- getContractAddress tx
    let newKernelCode = customKernelCode newContractAddress
        deployableKernelCode = makeDeployable newKernelCode
    let bsEncoded = B16.encode $ B.concat $ map toByteString deployableKernelCode
    (_, txK) <- deployContract sender bsEncoded
    kernelAddress <- getContractAddress txK
    print kernelAddress
    print newContractAddress

    (Right code) <- runWeb3 $ getCode kernelAddress Latest
    actualRunCode <- parseGoodExample $ fst $ B16.decode $ B.drop 2 $ encodeUtf8 code
    mapM_ print actualRunCode
    -- assertEqual "Actual run code should be the same as initial code" kernelCode actualRunCode
    (Right result) <- retrieveCaller newContractAddress
    (Right sender) <-  runWeb3 $ do
        accs <- accounts
        let sender = case accs of
                [] -> error "No accounts available"
                (a:_) -> a
        pure sender
    assertEqual "The caller should equal the kernel" ("0x" <> Address.toText sender) result
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

-- Take bytecode in a deployed format and make it deployable with no constructor
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

customKernelCode :: Address -> [OpCode]
customKernelCode address =
    [ PUSH1 (pack [0x1c]) -- outsize
    , DUP1 -- to be used by return later
    , PUSH1 (pack [0x00]) -- out
    , DUP1 -- to be used by return later
    , PUSH1 (pack [0x00]) -- insize
    , PUSH1 (pack [0x00]) -- in
    , PUSH20 addressBytes -- address
    , PUSH2 (pack [0xff,0xff]) -- gas
    , DELEGATECALL
    , PUSH1 (pack [0x1c]) -- outsize
    , PUSH1 (pack [0x00]) -- out
    , RETURN
    ]
    where
        (addressBytes,_) = B16.decode $ encodeUtf8 $ Address.toText $ address

returnCallerCode =
    [ CALLER
    , PUSH1 (pack [0x00])
    , MSTORE
    , PUSH1 (pack [0x14])
    , PUSH1 (pack [0x0c])
    , RETURN
    ]
