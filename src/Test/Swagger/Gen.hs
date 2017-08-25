{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Test.Swagger.Gen ( JsonHTTPRequest(..)
                        , generateRequestFromJsonDefinition) where

import           Control.Lens               hiding (elements)
import           Data.Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C
import qualified Data.HashMap.Strict.InsOrd as M
import           Data.Maybe
import           Data.Swagger
import           Network.HTTP.Types
import           System.FilePath.Posix      (joinPath)
import           Test.QuickCheck

-- |The FullyQualifiedHost contains the scheme (i.e. http://), hostname and port.
type FullyQualifiedHost = String

data JsonHTTPRequest = JsonHTTPRequest { requestHost    :: Maybe FullyQualifiedHost
                                       , requestMethod  :: Method
                                       , requestPath    :: String
                                       , requestHeaders :: RequestHeaders
                                       , requestBody    :: Maybe Value }
                                  deriving (Show, Eq)

-- |Given a swagger.json (encoded as a bytestring), decode it, and produce a
--  random Request that complies with the schema.
--  The return type is Either a parsing error (described as String), or a
--  random Request (in the IO monad because it's random).
generateRequestFromJsonDefinition :: BS.ByteString -> Either String (IO JsonHTTPRequest)
generateRequestFromJsonDefinition b = generateRequest <$> eitherDecodeStrict b

-- Generate a random request for a Swagger definition
generateRequest :: Swagger -> IO JsonHTTPRequest
generateRequest = generate . requestGenerator

-- Random Request generator
requestGenerator :: Swagger -> Gen JsonHTTPRequest
requestGenerator s = do (path, item) <- elements $ M.toList $ s ^. paths
                        let baseP = fromMaybe "/" $ s ^. basePath
                        let mHost = s ^. host
                        scheme <- elements $ fromMaybe [Https] $ s ^. schemes
                        pure $ JsonHTTPRequest (buildHost scheme <$> mHost)
                                                "POST"
                                                (joinPath [baseP, path])
                                                (catMaybes [(("Host",) . C.pack . (^. name)) <$> mHost])
                                                Nothing

    where
      buildHost :: Scheme -> Host -> String
      buildHost sc h = schemeToHttpPrefix sc ++ (h ^. name) ++ maybe "" ((':':) . show) (h ^. port)

      schemeToHttpPrefix Http  = "http://"
      schemeToHttpPrefix Https = "https://"
      schemeToHttpPrefix Ws    = "ws://"
      schemeToHttpPrefix Wss   = "wss://"
