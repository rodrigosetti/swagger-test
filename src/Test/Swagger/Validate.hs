{-# LANGUAGE OverloadedStrings #-}
module Test.Swagger.Validate ( module Test.Swagger.Types
                             , parseResponse
                             , validateResponseBytes
                             , validateResponseWithOperation
                             , validateResponse ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson                              hiding (Result)
import           Data.Attoparsec.ByteString              hiding (Result,
                                                          eitherResult, parse)
import qualified Data.Attoparsec.ByteString.Char8        as AC
import           Data.Attoparsec.ByteString.Lazy         hiding (Result)
import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as LBS
import           Data.CaseInsensitive
import           Data.Char                               (digitToInt)
import           Data.Generics
import qualified Data.HashMap.Strict.InsOrd              as M
import           Data.List
import           Data.Maybe
import           Data.Monoid                             ((<>))
import           Data.Swagger
import           Data.Swagger.Internal.Schema.Validation
import qualified Data.Text                               as T
import           Data.Text.Encoding
import           Network.HTTP.Types
import           Network.HTTP.Media
import           Test.Swagger
import           Test.Swagger.Types

-- |Validate a response, from a particular operation id, (encoded in a byte-string)
-- against a Swagger schema
validateResponseBytes :: LBS.ByteString -> Swagger -> OperationId -> Either String ()
validateResponseBytes input s opId =
  case parseResponse input of
    Left e         -> Left $ "could not parse HTTP response: " <> e
    Right response -> validateResponse response s opId

-- |Validate a response, from a particular operation id against a Swagger schema
validateResponse:: HttpResponse -> Swagger -> OperationId -> Either String ()
validateResponse res s opid =
    case maybeOp of
      Nothing        -> Left $ "operation not defined: " <> T.unpack opid
      Just operation -> validateResponseWithOperation res s operation
  where
   maybeOp = listToMaybe $ listify operationMatches s

   operationMatches :: Operation -> Bool
   operationMatches o = Just opid == o ^. operationId

-- |Validate a response, from a particular operation against a Swagger schema
validateResponseWithOperation :: HttpResponse -> Swagger -> Operation -> Either String ()
validateResponseWithOperation res s' operation =
        do let code = statusCode $ responseStatus res
               msr = M.lookup code (operation ^. responses.responses)
                    <|> operation ^. responses.default_

           sr <- maybe (fail $ "unspecified status code: " <> show code) pure (msr >>= refToMaybe)

           -- validate headers
           forM_ (M.toList $ sr ^. headers) $ uncurry $ \k h ->
                do hv <- maybe (fail $ "expected header: " <> T.unpack k) pure
                              $ lookup (mk k) $ responseHeaders res
                   validateWithParamSchema' (toJSON hv) $ h ^. paramSchema


           -- validate body
           case (sr ^. schema >>= refToMaybe, responseBody res) of
             (Nothing, Nothing) -> pure () -- no response expected, got no response (OK)
             (Just _, Nothing) -> fail $ "expected response body: " <> T.unpack (sr ^. description)
             (Nothing, Just _) -> fail "unexpected response body"
             (Just rs, Just bs) ->
                do jsonMime <- maybe (fail "unexpected!") pure $ parseAccept "application/json"

                   -- TODO: should default be JSON?
                   let respMime = fromMaybe jsonMime
                                   (lookup (mk "Content-Type") (responseHeaders res) >>= (parseAccept . encodeUtf8))

                   -- all possible content-types the operation can produce
                   let mimes = fromMaybe (s ^. produces) $ operation ^. produces

                   -- find one mime that matches
                   matchedMime <- maybe (fail "unexpected content-type") pure
                                $ find (matches respMime) (getMimeList mimes)

                   -- If JSON, validate
                   -- TODO: validate other non-JSON content-types
                   when (matches matchedMime jsonMime) $
                        do b <- eitherDecode bs
                           validateWithSchema' b rs

 where
   s = resolveReferences s'

   -- TODO: make it support patterns
   cfg = defaultConfig

   validateWithSchema' :: Value -> Schema -> Either String ()
   validateWithSchema' v = resultToEither . runValidation (validateWithSchema v) cfg

   validateWithParamSchema' :: Value -> ParamSchema t -> Either String ()
   validateWithParamSchema' v = resultToEither . runValidation (validateWithParamSchema v) cfg

   resultToEither :: Result a -> Either String a
   resultToEither (Failed es) = Left $ intercalate ", " es
   resultToEither (Passed a)  = Right a

-- |Parse a HttpResponse from ByteString
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
                    pure $ HttpResponse ver status hs body
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
