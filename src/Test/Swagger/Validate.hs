{-# LANGUAGE OverloadedStrings #-}
module Test.Swagger.Validate ( module Test.Swagger.Types
                             , parseResponse
                             , validateResponseBytes
                             , validateResponse ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString              hiding (eitherResult,
                                                          parse)
import qualified Data.Attoparsec.ByteString.Char8        as AC
import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as LBS
import           Data.CaseInsensitive
import           Data.Char                               (digitToInt)
import           Data.Swagger
import           Data.Swagger.Internal.Schema.Validation
import           Data.Text.Encoding
import           Network.HTTP.Types
import           Test.Swagger.Types

-- |Validate a response, from a particular operation id, (encoded in a byte-string)
-- against a Swagger schema
validateResponseBytes :: LBS.ByteString -> Swagger -> OperationId -> Maybe String
validateResponseBytes input s opId =
  case parseResponse input of
    Left e         -> Just e
    Right response -> validateResponse response s opId

-- |Validate a response, from a particular operation id, (encoded in a byte-string)
-- against a Swagger schema
validateResponse:: HttpResponse -> Swagger -> OperationId -> Maybe String
validateResponse _ _ _ = Nothing -- TODO: implement

parseResponse :: LBS.ByteString -> Either String HttpResponse
parseResponse = eitherResult . parse responseParser

responseParser :: Parser HttpResponse
responseParser = do ver <- versionParser <?> "http version"
                    skipHorizontalSpace1
                    status <- statusParser <?> "http status"
                    void endOfLine
                    hs <- headerParser `sepBy` endOfLine
                    body <- try (endOfLine >> endOfLine >> (Just <$> bodyParser)) <|> pure Nothing
                    endOfInput
                    pure $ HTTPResponse ver status hs body
    where
      endOfLine = string "\r\n" <|> string "\n"

      versionParser :: Parser HttpVersion
      versionParser = choice [ string "HTTP/0.9" >> pure http09
                             , string "HTTP/1.0" >> pure http10
                             , string "HTTP/1.1" >> pure http11 ]

      statusParser :: Parser Status
      statusParser = Status <$> (statusCodeParser <* skipHorizontalSpace1)
                            <*> takeTill (inClass "\r\n")

      skipHorizontalSpace1 = skipMany1 (skip $ inClass " \t")
      skipHorizontalSpace = skipMany (skip $ inClass " \t")

      headerParser :: Parser HttpHeader
      headerParser = do key <- BS.pack <$> many1 (satisfy $ notInClass " \t:")
                        skipHorizontalSpace >> string ":" >> skipHorizontalSpace
                        val <- BS.pack <$> many1 (satisfy $ notInClass "\r\n")
                        pure (mk $ decodeUtf8 key, decodeUtf8 val)

      bodyParser :: Parser LBS.ByteString
      bodyParser = takeLazyByteString

      statusCodeParser :: Parser Int
      statusCodeParser = do a <- digitToInt <$> AC.digit
                            b <- digitToInt <$> AC.digit
                            c <- digitToInt <$> AC.digit
                            pure $ fromIntegral $ a*100 + b*10 + c
