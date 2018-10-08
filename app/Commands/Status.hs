{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Commands.Status where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import qualified Data.ByteString.Base16 as B16 (decode)
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

runStatus :: Address -> IO ()
runStatus address = do
    void $ runWeb3 $ do
        Right sender <- getDefaultSender
        (res, keys) <- do
            let details = (Call {
                    callFrom = Just sender,
                    callTo = Just address,
                    callGas = Nothing,
                    callGasPrice = Nothing,
                    callValue = Nothing,
                    callData = Just ((JsonAbi.methodId (DFunction "listProcedures" False
                        [ ] (Just [FunctionArg "" "bytes24[]"]))))
                })
            theCall <- T.drop 2 <$> Eth.call details Latest
            let keys = parseProcs theCall
            theEffect <- Eth.sendTransaction details
            pure (theCall, keys)
        liftIO $ print keys
