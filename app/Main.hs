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
process (Compile input output capabilities) = do
  contents <- readFile input
  writeFile output (parseAndTransform capabilities contents)

parseAndTransform :: Maybe Capabilities -> String -> String
parseAndTransform capM input = case (capM, A.parse (parseOpCodes <* A.endOfInput) (B.pack input)) of
  (Just cap, A.Done _ ocs) -> toString (transform cap ocs)
  (_, A.Done _ ocs) -> toString ocs
  (_, _) -> "" --TODO: error handling
  where toString xs = B.unpack $ B.intercalate B.empty (map toByteString xs)
