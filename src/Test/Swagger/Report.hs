{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Test.Swagger.Report
Description : Exposes
Copyright   : (c) Rodrigo Setti, 2017
License     : BSD3
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Test.Swagger.Report ( TestReport(..)
                           , isSuccessful
                           , writeReportFile
                           , writeErrorReportFile
                           , runTests) where

import           Control.Lens                  ((^.))
import           Control.Monad
import           Data.Aeson                    as J
import qualified Data.ByteString.Lazy          as LBS
import           Data.Either
import           Data.Swagger                  as W
import           System.Random
import           Test.Swagger.Gen
import           Test.Swagger.Request
import           Test.Swagger.Types
import           Test.Swagger.Validate
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   as A
import           Data.List
import qualified Data.Set as S
import qualified Data.Text as T


-- |A description of a particular test run.
data TestReport = TestReport { reportSeed :: Seed
                             , reportOperation :: Operation
                             , reportRequest :: HttpRequest
                             , reportResponse :: HttpResponse
                             , reportResult :: ValidationResult }

-- |Predicate that tells whether or not a report is of a successful validation
isSuccessful :: TestReport -> Bool
isSuccessful TestReport { reportResult = Right _ } = True
isSuccessful _                              = False

isFailure :: TestReport -> Bool
isFailure = not . isSuccessful

instance ToJSON TestReport where
  toJSON (TestReport seed op req res vr) = J.object [ "seed" .= toJSON seed
                                                    , "operation" .= toJSON (op ^. operationId)
                                                    , "request" .= toJSON req
                                                    , "response" .= toJSON res
                                                    , "error" .= either toJSON (const Null) vr ]

-- |Run n tests for a 'Swagger' schema
runTests :: Swagger -> Size -> Int -> IO [TestReport]
runTests model size n =
    replicateM n $ do seed <- randomIO
                      let (op, req) = generateRequest seed size model Nothing
                      res <- doHttpRequest req
                      let vr = validateResponseWithOperation res model op
                      pure $ TestReport seed op req res vr

-- |Write a report file containing a header description about the schema, and
-- just a single error message. This is to be used if we find an error before
-- being able to run any test (parsing schema, etc.)
writeErrorReportFile :: FilePath -> Swagger -> String -> IO ()
writeErrorReportFile fp m = LBS.writeFile fp . renderHtml . errorReport m

-- |Write a report file containing a header description about the 'Swagger'
-- schema, then a section about each 'Operation', how many tests were performed,
-- general stats (# failures/successes) and request/response details for failures.
writeReportFile :: FilePath -> Swagger -> [TestReport] -> IO ()
writeReportFile fp m = LBS.writeFile fp . renderHtml . report m

errorReport :: Swagger -> String -> Html
errorReport model err = reportHeader model $ h2 $ toHtml err

report :: Swagger -> [TestReport] -> Html
report model reps =
    reportHeader model $ do
      let total = length reps
          totalFailures = length $ isFailure <$> reps

      dl ! class_ "header-stats" $ do
          dt "total number of tests"
          dd $ toHtml total
          dt "total number of failures"
          dd $ toHtml totalFailures

      -- group reports by operations
      let reportGroups = groupBy (\a b -> reportOperation a == reportOperation b) reps
      H.div ! class_ "operations" $
        forM_ reportGroups $ \case
            [] -> error "this shouldn't happen"
            group@(TestReport { reportOperation=op }:_) ->
              do hr
                 let total' = length group
                     totalFailures' = length $ isFailure <$> group
                 dl ! class_ "operation-header" $ do
                   forM_ (op ^. operationId) $ \i -> do
                     dt "operation id"
                     dd $ toHtml i
                   forM_ (op ^. W.summary) $ \s -> do
                     dt "summary"
                     dd $ toHtml s
                   forM_ (op ^. description) $ \d -> do
                     dt "description"
                     dd $ toHtml d
                   forM_ (op ^. tags) $ \t -> do
                     dt "tags"
                     dd $ toHtml $ T.intercalate " ," $ S.toList t
                   forM_ (op ^. deprecated) $ \d -> do
                     dt "deprecated"
                     dd $ toHtml d
                   dt "number of tests"
                   dd $ toHtml total'
                   dt "number of failures"
                   dd $ toHtml totalFailures'


reportHeader :: Swagger -> Html -> Html
reportHeader model inner =
  docTypeHtml $ do
       let schemaTitle = toHtml $ model ^. info . W.title
       H.head $
           H.title schemaTitle
       body $ do
           h1 schemaTitle
           forM_ (model ^. info.description) $ \d ->
             p ! class_ "schema-description" $ toHtml d
           H.div ! class_ "report-body" $ inner
