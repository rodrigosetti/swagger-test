{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Test.Swagger.Gen ( JsonHTTPRequest(..)
                        , generateRequestFromJsonDefinition) where

import           Control.Lens               hiding (elements)
import           Data.Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C
import qualified Data.HashMap.Strict.InsOrd as M
import           Data.List                  (partition)
import           Data.Maybe
import           Data.Monoid                ((<>))
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
                                       , requestBody    :: Maybe BS.ByteString }
                                  deriving (Show, Eq)

-- |Given a swagger.json (encoded as a bytestring), decode it, and produce a
--  random Request that complies with the schema.
--  The return type is Either a parsing error (described as String), or a
--  random Request (in the IO monad because it's random).
generateRequestFromJsonDefinition :: BS.ByteString -> Either String (IO JsonHTTPRequest)
generateRequestFromJsonDefinition = fmap generateRequest . eitherDecodeStrict

-- Generate a random request for a Swagger definition
generateRequest :: Swagger -> IO JsonHTTPRequest
generateRequest = generate . requestGenerator

-- Random Request generator
requestGenerator :: Swagger -> Gen JsonHTTPRequest
requestGenerator s = do let baseP = fromMaybe "/" $ s ^. basePath
                            mHost = s ^. host
                        -- pick a path
                        (path, item) <- elements $ M.toList $ s ^. paths
                        -- select one operation of the selected path
                        (method, operation) <- elements $ catMaybes [ (methodGet,) <$> item ^. get
                                                                    , (methodPut,) <$> item ^. put
                                                                    , (methodPost,) <$> item ^. post
                                                                    , (methodDelete,) <$> item ^. delete
                                                                    , (methodOptions,) <$> item ^. options
                                                                    , (methodHead,) <$> item ^. head_
                                                                    , (methodPatch,) <$> item ^. patch ]

                        let resolveRefParam :: Referenced Param -> Maybe Param
                            resolveRefParam (Inline p) = Just p
                            resolveRefParam (Ref r)    = M.lookup (getReference r) $ s ^. parameters

                        -- combine parameters common to all operations to parameters
                        -- specific to the selected operation
                        let params = catMaybes $ resolveRefParam <$> (item ^. parameters) <> (operation ^. parameters)
                            -- partition between required and non-required parameters
                            (requiredParams, notRequiredParams) = partition paramIsRequired params

                        selectedOptionalParams <- sublistOf notRequiredParams

                        let finalParams = requiredParams <> selectedOptionalParams

                        -- apply finalParams
                        -- TODO

                        scheme <- elements $ fromMaybe [Https] $ s ^. schemes
                        pure $ JsonHTTPRequest (buildHost scheme <$> mHost)
                                                method
                                                (joinPath [baseP, path])
                                                (catMaybes [(("Host",) . C.pack . (^. name)) <$> mHost])
                                                Nothing

    where
      buildHost :: Scheme -> Host -> String
      buildHost sc h = schemeToHttpPrefix sc <> (h ^. name) <> maybe "" ((':':) . show) (h ^. port)

      schemeToHttpPrefix Http  = "http://"
      schemeToHttpPrefix Https = "https://"
      schemeToHttpPrefix Ws    = "ws://"
      schemeToHttpPrefix Wss   = "wss://"

paramIsRequired :: Param -> Bool
paramIsRequired Param { _paramSchema = ParamOther ParamOtherSchema { _paramOtherSchemaIn = ParamPath}} = True
paramIsRequired p = fromMaybe False $ p ^. required
