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
          dtdd "total number of tests" total
          dtdd "total number of failures" totalFailures

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
                 h3 ! A.id (toValue opid)
                    $ a ! href ("#" <> toValue opid)
                    $ "Operation " <> toHtml opid
                 dl ! class_ "operation-header" $ do
                   forM_ (op ^. W.summary) $ \s ->
                     unless (T.null s)
                       $ dtdd "summary" s
                   forM_ (op ^. description) $ \d ->
                     unless (T.null d)
                        $ dtdd "description" d
                   unless (S.null $ op ^. tags) $
                     dtdd "tags"
                         $ T.intercalate " ," $ S.toList $ op ^. tags
                   forM_ (op ^. deprecated) $ \d ->
                     dtdd "deprecated" d
                   dtdd "number of tests" total'
                   dtdd "number of failures" totalFailures'
                 unless (null failing) $
                   H.div ! class_ "failures" $ do
                     h3 "Failure details"
                     forM_ failing $ \r -> do
                       let thisId = toValue opid <> toValue (reportSeed r)
                       h4 ! A.id thisId $
                         a ! href ("#" <> thisId)
                         $ "Seed " <> toHtml (reportSeed r)
                       H.div ! class_ "http-request" $ do
                         let thisId' = toValue opid <> toValue (reportSeed r) <> "-req"
                         h5 ! A.id thisId' $
                           a ! href ("#" <> thisId')
                           $ "HTTP Request"
                         code $ pre $ toHtml $ printRequest FormatHttp $ reportRequest r
                       H.div ! class_ "http-response" $ do
                         let thisId' = toValue opid <> toValue (reportSeed r) <> "-res"
                         h5 ! A.id thisId' $
                           a ! href ("#" <> thisId')
                           $ "HTTP Response"
                         code $ pre $ toHtml $ printResponse FormatHttp $ reportResponse r
                       let thisId' = toValue opid <> toValue (reportSeed r) <> "-err"
                       h5 ! A.id thisId' $
                         a ! href ("#" <> thisId')
                         $ "Error"
                       pre $ either toHtml (const "none") $ reportResult r


dtdd :: (ToMarkup a) => Html -> a -> Html
dtdd x y = dt x >> dd (toHtml y)

reportHeader :: NormalizedSwagger -> Html -> Html
reportHeader model inner =
  docTypeHtml $ do
       let s = getSwagger model
           schemaTitle = toHtml $ s ^. info . W.title
       H.head $ do
           H.title schemaTitle
           H.style "dl {\
                    \  margin: 0;\
                    \}\
                    \dl:after {\
                    \  content: '.';\
                    \  display: block;\
                    \  clear: both;\
                    \  visibility: hidden;\
                    \  overflow: hidden;\
                    \  height: 0;\
                    \}\
                    \dt {\
                    \  font-weight: bold;\
                    \ text-align: right;\
                    \  float: left;\
                    \  clear: left;\
                    \  width: 15%;\
                    \  margin-bottom: 1em;\
                    \}\
                    \dd {\
                    \  margin-left: 17%;\
                    \  margin-bottom: 1em;\
                    \}"
       body $ do
           h1 schemaTitle
           forM_ (s ^. info.description) $ \d ->
             p ! class_ "schema-description" $ toHtml d
           H.div ! class_ "report-body" $ inner
