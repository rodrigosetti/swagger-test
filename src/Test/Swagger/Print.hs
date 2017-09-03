{-# LANGUAGE OverloadedStrings #-}
module Test.Swagger.Print (Format(..)
                          , requestFormats
                          , responseFormats
                          , printRequest
                          , printResponse) where

import           Data.Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.CaseInsensitive
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text              as T
import           Data.Text.Encoding
import           Data.Text.Lazy.Builder
import           Network.HTTP.Types
import           Test.Swagger.Types

-- |Possible output formats that applies to 'HttpRequest' and 'HttpResponse'
-- values
data Format = FormatHttp | FormatCurl | FormatNone | FormatJSON
      deriving (Bounded, Enum)

instance Show Format where
  show FormatHttp = "http"
  show FormatCurl = "curl"
  show FormatNone = "none"
  show FormatJSON = "json"

requestFormats, responseFormats :: [Format]

-- |Valid output formats for 'HttpRequest' values
requestFormats = [minBound..]

-- |Valid output formats for 'HttpResponse' values
responseFormats = [FormatHttp, FormatJSON, FormatNone]

-- |Print a request according to format
printRequest :: Format -> HttpRequest -> Builder
printRequest FormatJSON r = fromUtf8Bytestring $ LBS.toStrict $ encode r
printRequest FormatNone _ = mempty
printRequest FormatHttp (HttpRequest _ method path query headers body) =
     fromUtf8Bytestring method
  <> fromText " "
  <> fromText path
  <> fromUtf8Bytestring (renderQuery True $ queryTextToQuery query)
  <> fromTextLn " HTTP/1.1"
  <> mconcat ((\(k,v) -> fromTextLn $ original k <> ": " <> v) <$> headers)
  <> case body of
       Just b  -> fromText "\n" <> fromUtf8Bytestring (LBS.toStrict b)
       Nothing -> mempty
printRequest FormatCurl (HttpRequest host method path query headers body) =
     fromText "curl -i"
  <> if method /= methodGet
      then fromUtf8Bytestring $ " -X " <> method
      else mempty
  <> fromText " '"
  <> fromText (escapeS host')
  <> fromText (escape path)
  <> fromText (escapeBS $ renderQuery True $ queryTextToQuery query)
  <> singleton '\''
  <> mconcat ((\(k,v) -> fromText $ " -H '" <> escape (original k) <> ": " <> escape v <> "'") <$> headers)
  <> case body of
       Just b  -> fromTextLn $ " -d '" <> escapeLBS b <> "'"
       Nothing -> singleton '\n'
   where
     host' = fromMaybe "http://localhost" host

     escapeLBS :: LBS.ByteString -> T.Text
     escapeLBS = escapeBS . LBS.toStrict

     escapeBS :: BS.ByteString -> T.Text
     escapeBS = escape . decodeUtf8

     escape :: T.Text -> T.Text
     escape = T.replace "'" "'\\''"

     escapeS :: String -> T.Text
     escapeS = escape . T.pack

-- |Print a response according to format
printResponse :: Format -> HttpResponse -> Builder
printResponse FormatCurl _ = error "unsupported format"
printResponse FormatJSON r = fromUtf8Bytestring $ LBS.toStrict $ encode r
printResponse FormatNone _ = mempty
printResponse FormatHttp r =
  let ver = responseHttpVersion r
      st = responseStatus r
      headers = responseHeaders r
  in
       fromString ("HTTP/" <> show (httpMajor ver) <> "." <> show (httpMinor ver) <> " ")
    <> fromString (show (statusCode st) <> " ")
    <> fromUtf8BytestringLn (statusMessage st)
    <> mconcat ((\(k,v) -> fromTextLn $ original k <> ": " <> v) <$> headers)
    <> case responseBody r of
         Just b  -> fromText "\n" <> fromUtf8Bytestring (LBS.toStrict b)
         Nothing -> mempty

fromUtf8Bytestring :: BS.ByteString -> Builder
fromUtf8Bytestring = fromText . decodeUtf8

fromUtf8BytestringLn :: BS.ByteString -> Builder
fromUtf8BytestringLn = fromTextLn . decodeUtf8

fromTextLn :: T.Text -> Builder
fromTextLn t = fromText t <> fromText "\n"
