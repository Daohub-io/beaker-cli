{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad.IO.Class

import Data.Attoparsec.ByteString
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

import Check.Stores
import OpCode.Exporter
import OpCode.Parser
import OpCode.Type
import Process
import OpCode.Utils
import CompileSolidity

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


runWeb3 :: Web3 a -> IO (Either Web3Error a)
runWeb3 = runWeb3' (HttpProvider "http://localhost:8545")

parseGoodExample bytecode =
    case parse (parseOpCodes <* endOfInput) bytecode `feed` "" of
        Fail i contexts err -> error $ "Opcodes should be parsed in full: " ++ show contexts ++ " " ++ err ++ " remaining: " ++ show (encode i)
        Partial f -> error $ show (f "")
        Done i r -> pure r


deployFromFile :: ([OpCode] -> [OpCode]) -> FilePath -> IO Address
deployFromFile transform filepath = do
    -- TODO: handle exceptions
    bsEncodedFull <- compileSolidityFileBinFull filepath
    bsEncodedRunTime <- compileSolidityFileBinRunTime filepath
    let
        bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
            in if remainder == B.empty then bytes else error (show remainder)
        bsDecodedRunTime = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedRunTime
            in if remainder == B.empty then bytes else error (show remainder)
    bytecode <- parseGoodExample bsDecodedFull :: IO [OpCode]
    let bsEncoded = B16.encode $ B.concat $ map toByteString $ transform bytecode
    newContractAddressRaw <- runWeb3 $ deployContractDefault bsEncoded
    case newContractAddressRaw of
        Right x -> pure x
        Left e -> error $ "Contract deployment failure: " ++ show e

deployFromFileId :: FilePath -> IO Address
deployFromFileId filepath = do
    -- TODO: handle exceptions
    bsEncodedFull <- compileSolidityFileBinFull filepath :: IO Text
    bsEncodedRunTime <- compileSolidityFileBinRunTime filepath
    let
        bsDecodedFull = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedFull
            in if remainder == B.empty then bytes else error (show remainder)
        bsDecodedRunTime = let (bytes, remainder) = B16.decode $ encodeUtf8 bsEncodedRunTime
            in if remainder == B.empty then bytes else error (show remainder)
    bytecode <- parseGoodExample bsDecodedFull :: IO [OpCode]
    let bsEncoded = encodeUtf8 bsEncodedFull :: B.ByteString
    newContractAddressRaw <- runWeb3 $ deployContractDefault bsEncoded
    case newContractAddressRaw of
        Right x -> pure x
        Left e -> error $ "Contract deployment failure: " ++ show e

getDefaultSender :: Web3 (Maybe Address)
getDefaultSender = do
    accs <- accounts
    pure $ case accs of
        [] -> Left "No accounts available"
        (a:_) -> Right a

deployContractDefault bsEncoded = do
    accs <- accounts
    let sender = case accs of
            [] -> error "No accounts available"
            (a:_) -> a
    (res, txH, tx, txR) <- deployContract' sender bsEncoded
    newContractAddressRaw <- getContractAddress' txH
    let newContractAddress = case newContractAddressRaw of
            Nothing -> error "contract no successfully deployed"
            Just x -> x
    code <- getCode newContractAddress Latest
    pure newContractAddress

deployContract sender bsEncoded =  do
    r <- runWeb3 $ do
        let details = Call {
                callFrom = Just sender,
                callTo = Nothing,
                callGas = Just 3000000,
                callGasPrice = Nothing,
                callValue = Nothing,
                callData = Just (T.pack $ C8.unpack $ "0x" `B.append` bsEncoded)
            }
        theCall <- Eth.call details Latest
        theEffect <- Eth.sendTransaction details
        pure (theCall, theEffect)
    case r of
        Left e -> error $ show e
        Right x -> pure x

-- deployContract' :: Address -> B.ByteString -> Web3 (Text, Text, Maybe Transaction, TxReceipt)
deployContract' sender bsEncoded = do
    let details = Call {
            callFrom = Just sender,
            callTo = Nothing,
            callGas = Just 3000000,
            callGasPrice = Nothing,
            callValue = Nothing,
            callData = Just (T.pack $ C8.unpack $ "0x" `B.append` bsEncoded)
        }
    theCall <- Eth.call details Latest
    -- liftIO $ print theCall
    txHash <- Eth.sendTransaction details
    -- liftIO $ putStrLn $ "tx: " ++ show txHash ++ " code: " ++ show (B.take 10 bsEncoded)
    -- liftIO $ print txHash
    -- TODO: wait for the transaction to be available
    tx <- blockingGetTransactionByHash txHash
    -- liftIO $ print tx
    txR <- blockingGetTransactionReceipt txHash
    -- liftIO $ print txR
    pure (theCall, txHash, tx, txR)

getContractAddress tx = do
    contractAddressResult <- runWeb3 $ do
        r <- blockingGetTransactionReceipt tx
        pure $ txrContractAddress r
    case contractAddressResult of
            Left e -> error ("ss" ++ show e)
            Right (Just x) -> pure x
            Right Nothing -> error "No new contract address was returned"

getContractAddress' tx = do
    r <- blockingGetTransactionReceipt tx
    pure $ txrContractAddress r

getAllLogs :: Address -> Web3 [Change]
getAllLogs contractAddress = do
    let details = (Filter
            { filterAddress = Just contractAddress
            , filterTopics    = Just []
            , filterFromBlock = Earliest
            , filterToBlock = Latest
            })
    theLogs <- Eth.getLogs details
    pure (theLogs)

blockingGetTransactionByHash txHash = do
    -- liftIO $ putStrLn $ "blockingGetTransactionByHash for: " ++ show txHash
    liftIO $ threadDelay 1000000
    r <- getTransactionByHash txHash
    -- liftIO $ print r
    case r of
        Nothing -> blockingGetTransactionByHash txHash
        Just x -> pure x

blockingGetTransactionReceipt txHash = blockingGetTransactionReceipt' 0 txHash
blockingGetTransactionReceipt' n txHash =
    if n > 20
        then error "Timed out waiting for transaction to be confirmed"
        else do
            -- liftIO $ putStrLn $ "blockingGetTransactionReceipt for: " ++ show txHash
            liftIO $ threadDelay 1000000
            r <- getTransactionReceipt txHash
            -- liftIO $ print r
            case r of
                Nothing -> blockingGetTransactionReceipt' (n+1) txHash
                Just x -> pure x

parseProcs theCall =
    let dataPosition = T.take (2*32) theCall
        length = T.take (2*32) $ T.drop (2*32) theCall
        keys = let
                dat = T.drop (2*2*32) theCall
                l = T.length dat
            in map (B.takeWhile ((/=) 0x0)) $ map fst $ map B16.decode $ map encodeUtf8 $ T.chunksOf 64 dat
    in keys
