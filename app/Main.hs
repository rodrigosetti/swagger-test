{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Command line application
Copyright   : (c) Rodrigo Setti, 2017
License     : BSD3
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Main (main) where

import           Control.Concurrent.Async
import           Control.Lens             ((^.))
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy     as LBS
import           Data.List
import           Data.Swagger             hiding (Format, info, version)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO        as LTIO
import           Data.Version             (showVersion)
import           Options.Applicative
import           Paths_swagger_test       (version)
import           System.Directory
import           System.Exit              (die)
import           System.FilePath.Posix
import           System.Random
import           Test.Swagger

-- |Program options
newtype Opts = Opts Command

data Command = Generate FilePath -- ^Swagger input file
                        (Maybe Seed)
                        (Maybe OperationId)
                        Format -- ^request output format
                        Bool -- ^output extra header info with seed and/or operation id
                        Size -- ^size parameter for the generation
             | Validate FilePath -- ^Swagger input file
                        (Maybe FilePath) -- ^read http response from file or stdin
                        OperationId
             | Request  FilePath -- ^Swagger input file
                        (Maybe Seed)
                        (Maybe OperationId)
                        Format -- ^request output format
                        Format -- ^response output format
                        Bool -- ^output extra header info with seed and/or operation id
                        Size -- ^size parameter for the generation
             | Report FilePath -- ^where to read the schemas
                      FilePath -- ^where to write the reports
                      Int -- ^number of tests to run
                      Size -- ^size parameter for the generation

opts :: Parser Opts
opts  = Opts <$> subparser ( command "generate" (info (generate <**> helper)
                                                      (progDesc "Generate a random request according to Schema"))
                           <> command "validate" (info (validate <**> helper)
                                                       (progDesc "Validate a response against Schema"))
                           <> command "request" (info (request <**> helper)
                                                      (progDesc "Generate, make the request, and validate response"))
                           <> command "report" (info (report <**> helper)
                                                      (progDesc "Run several tests and generate reports")))
  where
    generate :: Parser Command
    generate = Generate <$> schemaFileP
                        <*> seedP
                        <*> optional operationIdP
                        <*> option (formatReader requestFormats)
                                   ( metavar (intercalate "|" (map show requestFormats))
                                   <> help "output format of the HTTP request"
                                   <> long "request-format"
                                   <> value FormatHttp
                                   <> showDefault )
                        <*> infoP
                        <*> sizeP

    schemaFileP = strOption ( metavar "FILENAME"
                            <> long "schema"
                            <> short 's'
                            <> help "swagger JSON schema file to read from"
                            <> value "swagger.json"
                            <> showDefault)

    seedP = optional (option auto  ( metavar "N"
                                  <> help "specify the seed for the random generator"
                                  <> long "seed" ))

    infoP = switch (long "info"
                 <> short 'i'
                 <> help "render information about seed and operation id")

    sizeP = option auto ( metavar "N"
                      <> long "size"
                      <> help "control the size of the generated request"
                      <> value 30
                      <> showDefault )

    validate :: Parser Command
    validate = Validate <$> schemaFileP
                        <*> optional (strArgument ( metavar "FILENAME"
                                                  <> help "http response file to read from (default=stdin)" ))
                        <*> operationIdP

    request :: Parser Command
    request = Request <$> schemaFileP
                      <*> seedP
                      <*> optional operationIdP
                      <*> option (formatReader requestFormats)
                                 ( metavar (intercalate "|" (map show requestFormats))
                                 <> help "output format of the HTTP request"
                                 <> long "request-format"
                                 <> value FormatNone
                                 <> showDefault )
                      <*> option (formatReader responseFormats)
                                 (metavar (intercalate "|" (map show responseFormats))
                                 <> help "output format of the HTTP request"
                                 <> long "response-format"
                                 <> value FormatNone
                                 <> showDefault )
                      <*> infoP
                      <*> sizeP

    report :: Parser Command
    report = Report <$> strOption ( metavar "PATH"
                                    <> long "schemas"
                                    <> help "path to folder with swagger schemas"
                                    <> value "schemas"
                                    <> showDefault)
                        <*> strOption ( metavar "PATH"
                                    <> long "reports"
                                    <> help "path to folder to write the HTML reports"
                                    <> value "reports"
                                    <> showDefault)
                        <*> option auto ( metavar "N"
                                        <> long "tests"
                                        <> help "number of tests to run"
                                        <> value 100
                                        <> showDefault )
                        <*> sizeP

    operationIdP :: Parser OperationId
    operationIdP = T.pack <$> strOption (long "operation"
                                       <> short 'o'
                                       <> metavar "ID"
                                       <> help "specify a operation id to test (default pick randomly)")

    formatReader :: [Format] -> ReadM Format
    formatReader valids = maybeReader (\s -> find (\f -> show f == s) valids)

main :: IO ()
main = do Opts cmd <- customExecParser
                      (prefs $ disambiguate <> showHelpOnError <> showHelpOnEmpty)
                      optsInfo
          case cmd of
            Generate swaggerFile mseed mopid reqFmt renderInfo size ->
                do model <- readSwagger swaggerFile
                   void $ doGenerate model mseed mopid reqFmt renderInfo size
            Validate swaggerFile respFile opId ->
                do model <- readSwagger swaggerFile
                   respContents <- maybe LBS.getContents LBS.readFile respFile
                   case validateResponseBytes respContents model opId of
                     Left e  -> die $ "invalid: " <> e
                     Right _ -> putStrLn "valid"
            Request swaggerFile mseed mopid reqFmt resFmt renderInfo size ->
                do model <- readSwagger swaggerFile
                   (op, req) <- doGenerate model mseed mopid reqFmt renderInfo size
                   res <- doHttpRequest req
                   LTIO.putStrLn $ toLazyText $ printResponse resFmt res
                   case validateResponseWithOperation res model op of
                      Left e  -> die $ "invalid: " <> e
                      Right _ -> putStrLn "valid"
            Report schemasFolder reportFolder nTests size ->
                do createDirectoryIfMissing True reportFolder
                   schemaFiles <- listDirectory schemasFolder
                   forConcurrently_ schemaFiles $ \schemaFile ->
                      when (takeExtension schemaFile == ".json") $ do
                        let schemaFilePath = schemasFolder </> schemaFile
                            reportFile = reportFolder </> schemaFile -<.> "html"
                            reportDataFile = reportFolder </> schemaFile -<.> "results.json"
                        emodel <- eitherDecode <$> LBS.readFile schemaFilePath
                        case emodel of
                          Left e -> writeErrorReportFile reportFile $
                                      "Could not parse " <>  schemaFile <> ": " <> e
                          Right model ->
                            do reports <- runTests model nTests size
                               writeReportFile reportFile model reports
                               writeReportDataFile reportDataFile reports
  where
    readSwagger :: FilePath -> IO NormalizedSwagger
    readSwagger swaggerFile= do contents <- LBS.readFile swaggerFile
                                case eitherDecode contents of
                                  Left e  -> die e >> undefined
                                  Right m -> pure m

    optsInfo = info (opts <**> helper)
                    ( fullDesc
                    <> progDesc "Execute one of the commands available depending on your needs"
                    <> header ("Property-based testing tool for Swagger APIs - v. " <> showVersion version)
                    <> footer "Run `COMMAND --help` to get command specific options help")

    -- |Write a json of the structure data of the report
    writeReportDataFile :: FilePath -> [TestReport] -> IO ()
    writeReportDataFile fp reports =
      let failures = filter isFailure reports
      in LBS.writeFile fp $ encode failures

    doGenerate :: NormalizedSwagger
               -> Maybe Seed
               -> Maybe OperationId
               -> Format
               -> Bool
               -> Size
               -> IO (Operation, HttpRequest)
    doGenerate model mseed mopid reqFmt renderInfo size =
        do seed <- maybe (abs <$> randomIO) pure mseed
           let (op, req) = generateRequest seed size model mopid
           when renderInfo $
              TIO.putStrLn $ "# seed=" <> T.pack (show seed) <> maybe "" (" id=" <>) (op ^. operationId)
           LTIO.putStrLn $ toLazyText $ printRequest reqFmt req
           pure (op, req)
