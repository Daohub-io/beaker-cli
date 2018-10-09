{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Commands.Status where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
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

import Numeric.Natural

import Check.BeakerCompliance
import OpCode.Parser
import OpCode.Exporter
import OpCode.StructureParser
import OpCode.Type
import OpCode.Utils

import Text.Printf

import Utils

runStatus :: Address -> IO ()
runStatus address = do
    Right status <- runWeb3 $ do
        Right sender <- getDefaultSender
        theData <- do
            let details = (Call {
                    callFrom = Just sender,
                    callTo = Just address,
                    callGas = Nothing,
                    callGasPrice = Nothing,
                    callValue = Nothing,
                    callData = Just ((JsonAbi.methodId (DFunction "returnProcedureTable" False
                        [ ] (Just [FunctionArg "" "uint256[]"]))))
                })
            theCall <- T.drop 2 <$> Eth.call details Latest
            pure $ theCall
        pure theData
    -- This prints the raw table values
    -- case A.parseOnly (parseRawVals <* A.endOfInput) $ fst $ B16.decode $ T.encodeUtf8 status of
    --     Left e -> error $ show e
    --     Right (offset, length, vals) -> do
    --         print offset
    --         print length
    --         mapM_ (print . B16.encode) vals
    let theStatus = A.parseOnly (parseStatus <* A.endOfInput) $ fst $ B16.decode $ T.encodeUtf8 status
    let kernelStatus = case theStatus of
            Left e -> error e
            Right procTable -> KernelStatus
                { ks_address = address
                -- this filter is used to ignore null keys
                , ks_procedures = filter (\p->proc_key p /= (B.replicate 24 0x00)) procTable
                }
    putStrLn $ showKernelStatus kernelStatus

showKernelStatus :: KernelStatus -> String
showKernelStatus ks = "Kernel Instance: " ++ (show $ Address.toText $ ks_address ks) ++ "\n"
    ++ "  Procedures:\n"
    ++ showProcTable (ks_procedures ks)

showProcTable :: [Procedure] -> String
showProcTable procs = concat $ map showProc procs
    where
        showProc (Procedure key keyIndex location caps) = "    " ++ show keyIndex ++ ": \"" ++ C8.unpack key ++ "\" at " ++ "0x" ++ C8.unpack (B16.encode location) ++ "\n"
            ++ (concat $ map showCap caps)
        showCap (UnknownCap capType vals) = "      " ++ "0x" ++ (printf "%x" capType) ++ ": " ++ show vals ++ "\n"
        showCap (WriteCap location size) = "      " ++ "store_write: at " ++ printf "0x%x" location ++ " with " ++ show size ++ " additional keys\n"

data KernelStatus = KernelStatus
    { ks_address :: Address
    , ks_procedures :: [Procedure]
    } deriving (Show)

data Procedure = Procedure
    { proc_key :: B.ByteString
    , proc_keyIndex :: Integer
    , proc_location :: B.ByteString -- Address
    , proc_caps :: [Cap]
    } deriving (Show)

data Cap
    = UnknownCap Integer [B.ByteString]
    | WriteCap {- location -} Natural {- size -} Natural
    deriving (Show)

parseRawVals :: A.Parser (B.ByteString, Integer, [B.ByteString])
parseRawVals = do
    dataOffset <- A.take 32
    dataLength <- fromIntegral <$> evm256ToInteger <$> A.take 32
    vs <- A.manyTill (A.take 32) A.endOfInput
    pure (dataOffset, dataLength, vs)

parseStatus :: A.Parser [Procedure]
parseStatus = do
    dataOffset <- A.take 32
    dataLength <- fromIntegral <$> evm256ToInteger <$> A.take 32
    procs <- A.many' parseProc
    -- The rest of the table is zeroed out
    A.skipWhile ((==) 0x00)
    pure procs

parseProc :: A.Parser Procedure
parseProc = do
    key <- B.drop (32-24) <$> A.take 32
    keyIndex <- fromIntegral <$> evm256ToInteger <$> A.take 32
    location <- B.drop (32-20) <$> A.take 32
    nCaps <- fromIntegral <$> evm256ToInteger <$> A.take 32
    caps <- A.count nCaps parseCap
    pure $ Procedure key keyIndex location caps

parseCap :: A.Parser Cap
parseCap = do
    capLength <-fromIntegral <$> evm256ToInteger <$> A.take 32
    capType  <- fromIntegral <$> evm256ToInteger <$> A.take 32
    capValues <- A.count (capLength - 1) parseCapValue
    pure $ case capType of
        0x7 -> case capValues of
            [location, size] -> WriteCap (fromIntegral $ evm256ToInteger $ location) (fromIntegral $ evm256ToInteger $ size)
            _ -> error "invalid write cap"
        _ -> UnknownCap capType capValues

parseCapValue :: A.Parser B.ByteString
parseCapValue = A.take 32
