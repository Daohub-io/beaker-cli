module Commands.Deploy where

import Process

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as B16 (decode)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Check.BeakerCompliance
import OpCode.Parser
import OpCode.Exporter
import OpCode.StructureParser
import OpCode.Type

import Text.Printf

-- runDeploy :: ReadOpt -> FilePath -> IO ()
-- runDeploy readOpt inputPath = do
--     -- Read the file in
--     contents <- readContract readOpt inputPath
--     -- Parse it into plane opcodes (i.e. tokenise)
--     let
--         opcodes :: [OpCode]
--         opcodes = case parseOpCodesFromBS contents of
--             Left e -> error e
--             Right x -> x
--     -- Parse those opcodes into structured components
--     let
--         structure :: [StructuredCode]
--         structure = case fullStructuredParse inputPath opcodes of
--             Left e -> error $ show e
--             Right x -> x
--     -- Print out the structured components
--     mapM_ (printNonCompliance opcodes) $ findNonCompliances structure