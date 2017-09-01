{-# LANGUAGE OverloadedStrings #-}
module Test.Swagger.Print (Format(..)
                          , requestFormats
                          , responseFormats
                          , printRequest
                          , printResponse) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text            as T
import           Data.Text.Encoding
import qualified Data.Text.IO         as TIO
import           Network.HTTP.Types
import           Test.Swagger.Types

data Format = FormatHttp | FormatCurl | FormatNone | FormatJSON
      deriving (Bounded, Enum)

instance Show Format where
  show FormatHttp = "http"
  show FormatCurl = "curl"
  show FormatNone = "none"
  show FormatJSON = "json"

requestFormats, responseFormats :: [Format]
requestFormats = [minBound..]
responseFormats = [FormatHttp, FormatJSON, FormatNone]

-- Given a request and output format, render it correctly
printRequest :: Format -> HttpRequest -> IO ()
printRequest FormatJSON r = TIO.putStrLn $ decodeUtf8 $ LBS.toStrict $ encode r
printRequest FormatNone _ = pure ()
printRequest FormatHttp (HttpRequest _ method path query headers body) =
  do BS.putStr method
     putStr " "
     TIO.putStr path
     TIO.putStr $ decodeUtf8 $ renderQuery True $ queryTextToQuery query
     putStrLn " HTTP/1.1"
     forM_ headers $ \(k,v) -> TIO.putStr (original k) >> putStr ": " >> TIO.putStrLn v
     case body of
       Just b  -> putStr "\n" >> TIO.putStrLn (decodeUtf8 $ LBS.toStrict b)
       Nothing -> pure ()
printRequest FormatCurl (HttpRequest host method path query headers body) =
  do putStr "curl -i"
     when (method /= methodGet)
      $ BS.putStr $ " -X " <> method
     putStr " '"
     let host' = fromMaybe "http://localhost" host
     TIO.putStr $ escapeS host'
     TIO.putStr $ escape path
     TIO.putStr $ escapeBS $ renderQuery True $ queryTextToQuery query
     putChar '\''
     forM_ headers $ \(k,v) -> TIO.putStr (" -H '" <> escape (original k)) >> putStr ": " >> TIO.putStr (escape v <> "'")
     case body of
       Just b  -> TIO.putStrLn $ " -d '" <> escapeLBS b <> "'"
       Nothing -> putChar '\n'
   where
     escapeLBS :: LBS.ByteString -> T.Text
     escapeLBS = escapeBS . LBS.toStrict

     escapeBS :: BS.ByteString -> T.Text
     escapeBS = escape . decodeUtf8

     escape :: T.Text -> T.Text
     escape = T.replace "'" "'\\''"

     escapeS :: String -> T.Text
     escapeS = escape . T.pack


printResponse :: Format -> HttpResponse -> IO ()
printResponse FormatCurl _ = error "unsupported format"
printResponse FormatJSON r = TIO.putStrLn $ decodeUtf8 $ LBS.toStrict $ encode r
printResponse FormatNone _ = pure ()
printResponse FormatHttp r =
  do let ver = responseHttpVersion r
         st = responseStatus r
         headers = responseHeaders r
     putStr $ "HTTP/" <> show (httpMajor ver) <> "." <> show (httpMinor ver) <> " "
     putStr $ show (statusCode st) <> " "
     TIO.putStrLn $ decodeUtf8 $ statusMessage st
     forM_ headers $ \(k,v) -> TIO.putStr (original k) >> putStr ": " >> TIO.putStrLn v
     case responseBody r of
       Just b  -> putStr "\n" >> TIO.putStrLn (decodeUtf8 $ LBS.toStrict b)
       Nothing -> pure ()
