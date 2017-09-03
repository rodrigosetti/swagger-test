{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Concurrent.Async
import           Control.Lens                  ((^.))
import           Control.Monad
import           Data.Aeson                    as J
import qualified Data.ByteString.Lazy          as LBS
import           Data.List
import           Data.Maybe
import qualified Data.Set                      as S
import           Data.Swagger                  as W
import qualified Data.Text                     as T
import           System.Random
import           Test.Swagger.Gen
import           Test.Swagger.Print
import           Test.Swagger.Request
import           Test.Swagger.Types
import           Test.Swagger.Validate
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   as A
import Data.Monoid


-- |A description of a particular test run.
data TestReport = TestReport { reportSeed      :: Seed
                             , reportOperation :: Operation
                             , reportRequest   :: HttpRequest
                             , reportResponse  :: HttpResponse
                             , reportResult    :: ValidationResult }
                     deriving Eq

instance Ord TestReport where

  compare TestReport { reportSeed=s1, reportOperation=op1 }
          TestReport { reportSeed=s2, reportOperation=op2} =
    case (op1 ^. operationId, op2 ^. operationId) of
      (Nothing, Nothing) -> compare s1 s2
      (x, y) -> compare x y

-- |Predicate that tells whether or not a report is of a successful validation
isSuccessful :: TestReport -> Bool
isSuccessful TestReport { reportResult = Right _ } = True
isSuccessful _                                     = False

isFailure :: TestReport -> Bool
isFailure = not . isSuccessful

instance ToJSON TestReport where
  toJSON (TestReport seed op req res vr) = J.object [ "seed" .= toJSON seed
                                                    , "operation" .= toJSON (op ^. operationId)
                                                    , "request" .= toJSON req
                                                    , "response" .= toJSON res
                                                    , "error" .= either toJSON (const Null) vr ]

-- |Run n tests for a 'Swagger' schema
runTests :: NormalizedSwagger -> Int -> Size -> IO [TestReport]
runTests model n siz =
    replicateConcurrently n $
      do seed <- abs <$> randomIO
         let (op, req) = generateRequest seed siz model Nothing
         res <- doHttpRequest req
         let vr = validateResponseWithOperation res model op
         pure $ TestReport seed op req res vr

-- |Write a report file containing just a single error message. This is to be
-- used if we find an error before being able to run any test (parsing schema,
-- etc.)
writeErrorReportFile :: FilePath -> String -> IO ()
writeErrorReportFile fp = LBS.writeFile fp . renderHtml . errorReport "Error"

-- |Write a report file containing a header description about the 'Swagger'
-- schema, then a section about each 'Operation', how many tests were performed,
-- general stats (# failures/successes) and request/response details for failures.
writeReportFile :: FilePath -> NormalizedSwagger -> [TestReport] -> IO ()
writeReportFile fp m = LBS.writeFile fp . renderHtml . report m

errorReport :: String -> String -> Html
errorReport tit err =
  docTypeHtml $ do
       H.head $
           H.title $ toHtml tit
       body $ do
           h1 $ toHtml tit
           p ! class_ "error" $ toHtml err

report :: NormalizedSwagger -> [TestReport] -> Html
report model reps =
    reportHeader model $ do
      let total = length reps
          totalFailures = length $ filter isFailure reps

      dl ! class_ "header-stats" $ do
          dt "total number of tests"
          dd $ toHtml total
          dt "total number of failures"
          dd $ toHtml totalFailures

      -- group reports by operations
      let reportGroups = groupBy (\x y -> reportOperation x == reportOperation y)
                       $ sort reps
      H.div ! class_ "operations" $ do
        h2 "Operations"
        ul ! class_ "operations-menu" $
          forM_ reportGroups $ \case
              (TestReport { reportOperation=Operation { _operationOperationId=Just opid } }:_) ->
                li $ a ! href (toValue $ "#" <> opid) $ toHtml opid
              _ -> pure ()
        forM_ reportGroups $ \case
            [] -> error "this shouldn't happen"
            gr@(TestReport { reportOperation=op }:_) ->
              do hr
                 let total' = length gr
                     failing = filter isFailure gr
                     totalFailures' = length failing
                     opid = fromMaybe "" $ op ^. operationId
                 h3 ! A.id (toValue opid) $ "Operation " >> toHtml opid
                 dl ! class_ "operation-header" $ do
                   forM_ (op ^. W.summary) $ \s -> do
                     dt "summary"
                     dd $ toHtml s
                   forM_ (op ^. description) $ \d -> do
                     dt "description"
                     dd $ toHtml d
                   unless (S.null $ op ^. tags) $ do
                     dt "tags"
                     dd $ toHtml $ T.intercalate " ," $ S.toList $ op ^. tags
                   forM_ (op ^. deprecated) $ \d -> do
                     dt "deprecated"
                     dd $ toHtml d
                   dt "number of tests"
                   dd $ toHtml total'
                   dt "number of failures"
                   dd $ toHtml totalFailures'
                 unless (null failing) $
                   H.div ! class_ "failures" $ do
                     h3 "Failures"
                     forM_ failing $ \r -> do
                       dl ! class_ "test-details" $ do
                         dt "seed"
                         dd $ toHtml $ reportSeed r
                         dt "error"
                         dt $ either toHtml (const "none") $ reportResult r
                       H.div ! class_ "http-request" $ do
                         h4 "HTTP Request"
                         pre $ toHtml $ printRequest FormatHttp $ reportRequest r
                       H.div ! class_ "http-response" $ do
                         h4 "HTTP Response"
                         pre $ toHtml $ printResponse FormatHttp $ reportResponse r



reportHeader :: NormalizedSwagger -> Html -> Html
reportHeader model inner =
  docTypeHtml $ do
       let s = getSwagger model
           schemaTitle = toHtml $ s ^. info . W.title
       H.head $
           H.title schemaTitle
       body $ do
           h1 schemaTitle
           forM_ (s ^. info.description) $ \d ->
             p ! class_ "schema-description" $ toHtml d
           H.div ! class_ "report-body" $ inner
