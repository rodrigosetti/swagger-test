{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.CaseInsensitive  (original)
import           Data.Maybe            (fromMaybe)
import           Data.Semigroup        ((<>))
import qualified Data.Text             as T
import           Data.Text.Encoding
import qualified Data.Text.IO          as TIO
import           Network.HTTP.Types
import           Options.Applicative
import           System.Exit           (die)
import           System.Random
import           Test.Swagger.Gen
import           Test.Swagger.Validate

-- |Program options
data Opts = Opts Command
                 FilePath -- ^Swagger input file

data Command = Generate (Maybe Seed)
                        (Maybe OperationId)
                        OutputFormat
                        Bool -- ^output extra header info with seed and/or operation id
             | Validate (Maybe FilePath) -- ^read http response from file or stdin
                        OperationId

data OutputFormat = OutputHttp | OutputCurl

instance Show OutputFormat where
  show OutputHttp = "http"
  show OutputCurl = "curl"

opts :: Parser Opts
opts  = Opts <$> subparser ( command "generate" (info (generate <**> helper) (progDesc "Generate a request"))
                           <> command "validate" (info (validate <**> helper) (progDesc "Validate a response")))
             <*> strArgument ( metavar "swagger.json"
                             <> help "swagger spec to read from"
                             <> value "swagger.json"
                             <> showDefault)
  where
    generate :: Parser Command
    generate = Generate <$> optional (option auto  ( metavar "N"
                                              <> help "specify the seed for the random generator"
                                              <> long "seed"
                                              <> short 's' ))
                        <*> optional operationId
                        <*> option outputFormatReader ( metavar "http|curl"
                                                       <> help "output format of the HTTP request"
                                                       <> long "output-format"
                                                       <> value OutputHttp
                                                       <> showDefault )
                        <*> switch (long "info"
                                   <> short 'i'
                                   <> help "render information about seed and operation id")

    validate :: Parser Command
    validate = Validate <$> optional (strOption ( metavar "FILENAME"
                                         <> help "http response file to read from (default=stdin)"
                                      ))
                        <*> operationId

    operationId :: Parser OperationId
    operationId = T.pack <$> strOption (long "operation"
                                       <> short 'o'
                                       <> metavar "ID"
                                       <> help "specify a operation id to test (default pick randomly)")

    outputFormatReader :: ReadM OutputFormat
    outputFormatReader = maybeReader (`lookup` table)
      where
        table = [(show o, o) | o <- [OutputHttp, OutputCurl]]

main :: IO ()
main = do Opts cmd swaggerFile <- execParser optsInfo
          contents <- LBS.readFile swaggerFile
          case eitherDecode contents of
            Left e -> die e
            Right schema ->
              case cmd of
                Generate mseed mopid ofmt renderInfo ->
                    do seed <- maybe randomIO pure mseed
                       let req = generateRequest seed schema mopid
                           opid = requestOperationId req
                       when renderInfo $
                          TIO.putStrLn $ "# seed=" <> T.pack (show seed) <> maybe "" (\i -> " id=" <> i) opid
                       printRequest ofmt req
                Validate respFile opId ->
                    do respContents <- maybe LBS.getContents LBS.readFile respFile
                       print $ parseResponse respContents

  where
    optsInfo = info (opts <**> helper)
                    (fullDesc
                    <> progDesc "Generate Swagger requests and validates responses"
                    <> header "Testing tool for Swagger APIs")

-- Given a request and output format, render it correctly
printRequest :: OutputFormat -> HTTPRequest -> IO ()
printRequest OutputHttp (HTTPRequest _ _ method path query headers body) =
  do BS.putStr method
     putStr " "
     TIO.putStr path
     TIO.putStr $ decodeUtf8 $ renderQuery True $ queryTextToQuery query
     putStrLn " HTTP/1.1"
     forM_ headers $ \(k,v) -> TIO.putStr (original k) >> putStr ": " >> TIO.putStrLn v
     case body of
       Just b  -> putStr "\n" >> TIO.putStrLn (decodeUtf8 $ LBS.toStrict b)
       Nothing -> pure ()
printRequest OutputCurl (HTTPRequest _ host method path query headers body) =
  do putStr "curl -i"
     when (method /= methodGet)
      $ BS.putStr $ " -X " <> method
     putStr " '"
     let host' = fromMaybe "http://localhost" host
     putStr host'
     TIO.putStr path
     TIO.putStr $ decodeUtf8 $ renderQuery True $ queryTextToQuery query
     putChar '\''
     forM_ headers $ \(k,v) -> TIO.putStr (" -H '" <> original k) >> putStr ":" >> TIO.putStr (v <> "'")
     case body of
       Just b  -> TIO.putStrLn $ " -d '" <> decodeUtf8 (LBS.toStrict b) <> "'"
       Nothing -> putChar '\n'
