module Commands where

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

runOpCodes :: ReadOpt -> FilePath -> IO ()
runOpCodes readOpt inputPath = do
    -- Read the file in
    contents <- readContract readOpt inputPath
    -- Parse it into plane opcodes (i.e. tokenise)
    let
        opcodes :: [OpCode]
        opcodes = case parseOpCodesFromBS contents of
            Left e -> error e
            Right x -> x
    mapM_ printE $ zip (scanl (\offset opcode -> offset + (nBytes opcode)) 0 opcodes) opcodes
    where
        printE (offset, opcode) = printf "0x%x: %s\n" offset (show opcode)

runStructures :: ReadOpt -> FilePath -> IO ()
runStructures readOpt inputPath = do
    -- Read the file in
    contents <- readContract readOpt inputPath
    -- Parse it into plane opcodes (i.e. tokenise)
    let
        opcodes :: [OpCode]
        opcodes = case parseOpCodesFromBS contents of
            Left e -> error e
            Right x -> x
    -- Parse those opcodes into structured components
    let
        structure :: [StructuredCode]
        structure = case fullStructuredParse inputPath opcodes of
            Left e -> error $ show e
            Right x -> x
    -- Print out the structured components
    mapM_ print structure

runCheck :: ReadOpt -> FilePath -> IO ()
runCheck readOpt inputPath = do
    -- Read the file in
    contents <- readContract readOpt inputPath
    -- Parse it into plane opcodes (i.e. tokenise)
    let
        opcodes :: [OpCode]
        opcodes = case parseOpCodesFromBS contents of
            Left e -> error e
            Right x -> x
    -- Parse those opcodes into structured components
    let
        structure :: [StructuredCode]
        structure = case fullStructuredParse inputPath opcodes of
            Left e -> error $ show e
            Right x -> x
    -- Print out the structured components
    mapM_ (printNonCompliance opcodes) $ findNonCompliances structure

runCompile :: FilePath -> FilePath -> Maybe Capabilities -> IO ()
runCompile inputFile outputFile capabilities = do
    contents <- B.readFile inputFile
    let result = parseAndTransform capabilities contents
    case result of
        Left err -> putStrLn err
        Right output -> B.writeFile outputFile output

parseAndTransform :: Maybe Capabilities -> B.ByteString -> Either String B.ByteString
parseAndTransform capM input = case (capM, A.parseOnly (parseOpCodes <* A.endOfInput) input) of
    (Just cap, Right ocs) -> Right . toString $ transform cap ocs
    (_, Right ocs) -> Right . toString $ ocs
    (_, Left err) -> Left err
    where toString xs = B.intercalate B.empty (map toByteString xs)

parseOpCodesFromBS :: B.ByteString -> Either String [OpCode]
parseOpCodesFromBS input = A.parseOnly (parseOpCodes <* A.endOfInput) input

data ReadOpt = ReadBinary | ReadHex | ReadSolC deriving (Show, Eq, Read)

readContract :: ReadOpt -> FilePath -> IO B.ByteString
readContract ReadBinary filePath = B.readFile filePath
readContract ReadHex filePath = hexToBinary <$> T.readFile filePath
readContract ReadSolC filePath = hexToBinary <$> solcToHex <$> T.readFile filePath

hexToBinary :: T.Text -> B.ByteString
hexToBinary contents =
    let (bsEncoded,rem) = B16.decode  $ T.encodeUtf8 contents
    in if rem == B.empty then bsEncoded else error "decoding failed"

solcToHex :: T.Text -> T.Text
solcToHex contents =
    let hexCode = if length (T.lines contents) < 4 then error (show contents) else T.filter (/= '\r') ((T.lines contents) !! 3)
    in T.filter (/= '\r') hexCode
