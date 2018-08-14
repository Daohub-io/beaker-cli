module Main where

import Process
import Options.Applicative
import Data.Semigroup ((<>))
import Data.String (IsString)
import Numeric.Natural
import OpCode.Parser
import OpCode.Exporter

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as B

data Compile = Compile
    { input :: FilePath
    , output :: FilePath
    , capabilities :: Maybe Capabilities
    } deriving Show

capParser :: Parser Capabilities
capParser = Capabilities
    <$> option auto (
        long "write" <> help "write capabilities"
    )

cliParser :: Parser Compile
cliParser = Compile
    <$> strArgument (
        metavar "INPUT" <> help "input abi file"
    )
    <*> strArgument (
        metavar "OUTPUT"
        <> value "output.abi"
        <> help "output abi file"
    )
    <*> optional capParser

main :: IO ()
main = do
    cmd <- (execParser opts)
    process cmd
        where opts = info (cliParser <**> helper) fullDesc

process :: Compile -> IO ()
process (Compile inputFile outputFile capabilities) = do
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


