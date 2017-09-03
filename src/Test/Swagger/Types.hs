{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Test.Swagger.Types
Description : Types used for other swagger-test modules
Copyright   : (c) Rodrigo Setti, 2017
License     : BSD3
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX

This module exposes some types that ure used across other modules
of swagger-test.
-}
module Test.Swagger.Types (FullyQualifiedHost
                          , Seed
                          , Size
                          , OperationId
                          , HttpHeader
                          , Headers
                          , HttpRequest(..)
                          , HttpResponse(..)
                          , resolveReferences
                          , refToMaybe) where

import           Control.Arrow
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive
import qualified Data.HashMap.Lazy    as HM
import qualified Data.Text            as T
import           Data.Text.Encoding
import           Network.HTTP.Types
import           Control.Lens ((^.))
import           Data.Generics
import qualified Data.HashMap.Strict.InsOrd as M
import           Data.Monoid                ((<>))
import           Data.Swagger

-- |The FullyQualifiedHost contains the scheme (i.e. http://), hostname and port.
type FullyQualifiedHost = String

type Seed = Int
type Size = Int
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
      headersMap = HM.fromList $ first original <$> requestHeaders r

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


-- |Replace all references with inlines
resolveReferences :: Swagger -> Swagger
resolveReferences s = everywhere' (mkT resolveSchema) $ everywhere' (mkT resolveParam) s
  -- NOTE: we need to use the top-down everywhere variant for this to work as intented
  where
    resolveParam :: Referenced Param -> Referenced Param
    resolveParam i@Inline {} = i
    resolveParam (Ref (Reference r))  = maybe (error $ "undefied schema: " <> T.unpack r) Inline
                                      $ M.lookup r $ s ^. parameters
    resolveSchema :: Referenced Schema -> Referenced Schema
    resolveSchema i@Inline {} = i
    resolveSchema (Ref (Reference r)) = maybe (error $ "undefied schema: " <> T.unpack r) Inline
                                      $ M.lookup r $ s ^. definitions

-- |Transform a reference into a Just value if is inline, Nothing, otherwise
refToMaybe :: Referenced a -> Maybe a
refToMaybe (Inline i) = Just i
refToMaybe (Ref _)    = Nothing
