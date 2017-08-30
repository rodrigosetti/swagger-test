module Main (main) where

import           Control.Monad        (forM_)
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive (original)
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup       ((<>))
import           Data.Text.Encoding
import qualified Data.Text.IO         as TIO
import           Network.HTTP.Types
import           Options.Applicative
import           System.Exit          (die)
import           System.Random
import           Test.Swagger.Gen

-- |Program options
data Opts = Opts Command
                 (Maybe FilePath) -- ^Swagger input file or stdin

type Seed = Int
type OperationId = String

data Command = Generate (Maybe Seed) (Maybe OperationId)
             | Validate Bool -- ^include headers
                        OperationId

opts :: Parser Opts
opts  = Opts <$> subparser ( command "generate" (info (generate <**> helper) (progDesc "Generate a request"))
                           <> command "validate" (info (validate <**> helper) (progDesc "Validate a response")))
             <*> optional (strArgument ( metavar "FILENAME"
                                       <> help "swagger spec to read from (swagger.json). default: stdin"
                                       ))
  where
    generate :: Parser Command
    generate = Generate <$> optional (option auto  ( metavar "N"
                                              <> help "specify the seed for the random generator"
                                              <> long "seed"
                                              <> short 's' ))
                        <*> optional operationId

    validate :: Parser Command
    validate = Validate <$> switch ( help "whether to expect headers to be validated"
                                   <> long "headers"
                                   <> short 'h' )
                        <*> operationId

    operationId :: Parser OperationId
    operationId = strOption (long "operation"
                           <> short 'o'
                           <> metavar "ID"
                           <> help "specify a operation id to test (default pick randomly)")

main :: IO ()
main = do Opts cmd inputFile <- execParser optsInfo
          contents <- maybe LBS.getContents LBS.readFile inputFile
          case eitherDecode contents of
            Left e -> die e
            Right schema ->
              case cmd of
                Generate mseed _ ->
                    do seed <- maybe getAndReportNewSeed pure mseed
                       printRequest $ generateRequest seed schema
                Validate _ _ -> error "not implemented"
  where
    optsInfo = info (opts <**> helper)
                    (fullDesc
                    <> progDesc "Generate Swagger requests and validates responses"
                    <> header "Testing tool for Swagger APIs")

    getAndReportNewSeed = do seed <- randomIO
                             putStr "// using seed: "
                             print seed
                             pure seed

printRequest :: HTTPRequest -> IO ()
printRequest (HTTPRequest opId host method path query headers body) =
  do maybe (pure ()) (putStrLn . ("// operation id: " <>)) opId
     BS.putStr method
     putStr " "
     putStr $ fromMaybe "" host
     TIO.putStr path
     TIO.putStrLn $ decodeUtf8 $ renderQuery True $ queryTextToQuery query
     forM_ headers $ \(k,v) -> TIO.putStr (original k) >> putStr ": " >> TIO.putStrLn v
     case body of
       Just b  -> putStr "\n" >> TIO.putStrLn (decodeUtf8 $ LBS.toStrict b)
       Nothing -> pure ()
