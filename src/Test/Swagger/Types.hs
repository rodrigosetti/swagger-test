{-# LANGUAGE OverloadedStrings #-}
module Test.Swagger.Types (FullyQualifiedHost
                          , Seed
                          , OperationId
                          , HttpHeader
                          , Headers
                          , HttpRequest(..)
                          , HttpResponse(..)) where

import           Control.Arrow
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive
import qualified Data.HashMap.Lazy    as M
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Network.HTTP.Types

-- |The FullyQualifiedHost contains the scheme (i.e. http://), hostname and port.
type FullyQualifiedHost = String

type Seed = Int
type OperationId = T.Text

type HttpHeader = (CI T.Text, T.Text)
type Headers = [HttpHeader]

data HttpRequest = HttpRequest { requestHost    :: Maybe FullyQualifiedHost
                               , requestMethod  :: Method
                               , requestPath    :: T.Text
                               , requestQuery   :: QueryText
                               , requestHeaders :: Headers
                               , requestBody    :: Maybe LBS.ByteString }
                      deriving (Show)

instance ToJSON HttpRequest where
  toJSON r = object [ "host" .= toJSON (requestHost r)
                    , "method" .= toJSON (decodeUtf8 $ requestMethod r)
                    , "path" .= toJSON (requestPath r)
                    , "query" .= toJSON (requestQuery r)
                    , "headers" .= toJSON headersMap
                    , "body" .= toJSON (decodeUtf8 . LBS.toStrict <$> requestBody r) ]
    where
      headersMap = M.fromList $ first original <$> requestHeaders r

data HttpResponse = HttpResponse { responseHttpVersion :: HttpVersion
                                 , responseStatus      :: Status
                                 , responseHeaders     :: Headers
                                 , responseBody        :: Maybe LBS.ByteString }
                      deriving (Show)

instance ToJSON HttpResponse where
  toJSON r = object [ "version" .= object [ "major" .= toJSON (httpMajor ver)
                                          , "minor" .= toJSON (httpMinor ver)]
                    , "status" .= object [ "code" .= toJSON (statusCode st)
                                         , "message" .= toJSON (decodeUtf8 $ statusMessage st) ]
                    , "headers" .= toJSON headersMap
                    , "body" .= toJSON (decodeUtf8 . LBS.toStrict <$> responseBody r) ]
    where
      ver = responseHttpVersion r
      st = responseStatus r
      headersMap = M.fromList $ first original <$> responseHeaders r
