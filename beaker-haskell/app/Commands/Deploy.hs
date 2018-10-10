{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Commands.Deploy where

import Process

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Network.Ethereum.Web3 hiding (runWeb3)
import Network.Ethereum.Web3.Web3
import Network.Ethereum.Web3.JsonAbi as JsonAbi
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Eth as Eth
import qualified Network.Ethereum.Web3.Address as Address

import Check.BeakerCompliance
import OpCode.Parser
import OpCode.Exporter
import OpCode.StructureParser
import OpCode.Type

import Text.Printf

import Utils

import Paths_beaker_cli

-- Currently set to run an example for evaluation purposes.
runDeploy :: IO ()
runDeploy = void example

runDeployNormal :: IO ()
runDeployNormal = do
    kernelCode <- getKernelCode
    newContractAddress <- runWeb3 $ do
        accs <- accounts
        let sender = case accs of
                [] -> error "No accounts available"
                (a:_) -> a
        (res, txH, tx, txR) <- deployContract' sender kernelCode
        newContractAddressRaw <- getContractAddress' txH
        let newContractAddress = case newContractAddressRaw of
                Nothing -> error "contract not successfully deployed"
                Just x -> x
        pure newContractAddress
    case newContractAddress of
        Left e -> error $ show e
        Right address ->
            T.putStrLn $ "Kernel Instance Address: 0x"
                <> Address.toText address

-- compiledKernelPath = "../Kernel.bin/Kernel.bin"
-- TODO: currently a bit of a hack
getKernelCode :: IO B.ByteString
getKernelCode = do
    filePath <- getDataFileName "Kernel.bin/Kernel.bin"
    B.readFile filePath

example = runWeb3 $ do
    -- Get the account we will be using
    (Right sender) <- getDefaultSender
    -- Get the kernel code from file
    kernelCode <- liftIO getKernelCode
    -- Deploy the kernel and receive its address
    kernelInstanceAddress <- do
        (res, txH, tx, txR) <- deployContract' sender kernelCode
        newContractAddressRaw <- getContractAddress' txH
        let newContractAddress = case newContractAddressRaw of
                Nothing -> error "contract not successfully deployed"
                Just x -> x
        pure newContractAddress
    liftIO $ T.putStrLn $ "Kernel Instance Address: "
                                        <> Address.toText kernelInstanceAddress

    -- Deploy an example procedure
    -- TODO: abstract away this procedure creation
    filePath <- liftIO $ getDataFileName "test/Models/Adder.hexbinbuild"
    bsEncoded <- liftIO $ B.readFile filePath
    deployAndRegisterProcedure sender kernelInstanceAddress "uhatone" bsEncoded

deployAndRegisterProcedure sender kernelInstanceAddress procKey bsEncoded = do
    let
        oCode = (T.pack $ C8.unpack bsEncoded)

        paddedOCode = oCode <> T.replicate oCodePaddingLength "0"
            where oCodePaddingLength = 64 - (T.length oCode `mod` 64)
        nElementsInCode = T.pack $ printf "%x" $ T.length oCode `div` 2 :: Text
        elementsInOCode = T.replicate (64 - (T.length nElementsInCode `mod` 64)) "0" <> nElementsInCode

        oCodeOffsetLocation = "0000000000000000000000000000000000000000000000000000000000000060"
        -- |The offset of the first byte after the oCode data, in bytes from
        -- the beginning of the arguments section.
        oCodeOffset = (T.length (elementsInOCode <> paddedOCode) `div` 2) + 0x60
        oCodeOffsetText = T.pack $ printf "%x" oCodeOffset :: Text
        oCodeOffsetTextPadded = T.replicate (64 - (T.length oCodeOffsetText `mod` 64)) "0" <> oCodeOffsetText

        encodedProcKey = T.decodeUtf8 $ B16.encode $ padAfter 32 $ T.encodeUtf8 procKey
        -- capsOffset =

        details = (Call {
                callFrom = Just sender,
                callTo = Just kernelInstanceAddress,
                callGas = Nothing,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just ((JsonAbi.methodId (DFunction "createProcedure" False
                    [ FunctionArg "name" "bytes24"
                    , FunctionArg "oCode" "bytes"
                    , FunctionArg "caps" "uint256[]"
                    --                                                228                             bytes24 - name            offset to oCode (bytes) 3*32                             offset  to caps (uint256[])  number of elements in oCode                                               number of cap elements                                                                                                                                                                                                                                                                                                                                                                                                                                                 padding                                                 number of elements in caps
                    ] (Just [FunctionArg "err" "uint8", FunctionArg "procedureAddress" "address"])))
                    -- single bytes24 parameter (the procedure key)
                    <> encodedProcKey
                    -- The second parameter is the oCode parameter. As this is
                    -- dynamic we simply record the offset of the data location
                    -- from the beginning of the arguments (in bytes). This is
                    -- a single 32-byte value
                    <> oCodeOffsetLocation
                    -- The same with the location of the caps
                    <> oCodeOffsetTextPadded

                    -- This is the oCode data section. First is the length in
                    -- bytes, then comes the oCode
                    <> elementsInOCode
                    <> paddedOCode

                    -- This is the caps data section. First is the length. As
                    -- that is zero, there is nothing after it.
                    <> "0000000000000000000000000000000000000000000000000000000000000004"
                    <> "0000000000000000000000000000000000000000000000000000000000000003"
                    <> "0000000000000000000000000000000000000000000000000000000000000007"
                    <> "0000000000000000000000000000000000000000000000000000000000008000"
                    <> "0000000000000000000000000000000000000000000000000000000000000001"
                    )
            })
    (res@(_,procedureAddressRaw),theEffect,tx,txR) <- do
        theCall <- T.drop 2 <$> Eth.call details Latest
        theEffect <- Eth.sendTransaction details
        tx <- blockingGetTransactionByHash theEffect
        txR <- blockingGetTransactionReceipt theEffect
        let err = T.take (32*2) theCall
            procedureAddress = T.drop ((32-20)*2) $ T.drop (32*2) $ theCall
        pure ((err, procedureAddress), theEffect, tx, txR)
    let procedureAddress = case Address.fromText procedureAddressRaw of
            Left e -> error ("procedureAddress was not retrieved: " ++ show e)
            Right addr -> addr
    if (procedureAddress /= Address.zero)
        then liftIO $ T.putStrLn $ "Example Procedure Address: "
            <> Address.toText procedureAddress
        else error "not a valid address"