module Main where

import Process
import Options.Applicative
import Data.Semigroup ((<>))
import Data.String (IsString)
import Numeric.Natural

data Compile = Compile 
  { input :: FilePath
  , output :: FilePath
  , capabilities :: Maybe Capabilities
  } deriving Show

capParser :: Parser Capabilities
capParser = Capabilities 
  <$> option auto (long "write" <> help "write capabilities")

cliParser :: Parser Compile
cliParser = Compile
  <$> strOption (metavar "INPUT" <> help "input abi file")
  <*> strOption (
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
process c@(Compile input output (Just capabilities)) = print c
process _ = pure ()