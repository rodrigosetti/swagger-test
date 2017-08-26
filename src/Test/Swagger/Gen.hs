{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Test.Swagger.Gen ( HTTPRequest(..)
                        , generateRequestFromJsonDefinition) where

import           Control.Applicative        ((<|>))
import           Control.Arrow              ((&&&))
import           Control.Lens               hiding (elements)
import           Data.Aeson
import           Data.Binary.Builder
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as LBS
import           Data.CaseInsensitive       (mk)
import qualified Data.HashMap.Strict.InsOrd as M
import           Data.List                  (partition)
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Swagger
import           Data.Swagger.Internal      (SwaggerKind (..))
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Network.HTTP.Types
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.URI
import           System.FilePath.Posix      (joinPath)
import           Test.QuickCheck

-- |The FullyQualifiedHost contains the scheme (i.e. http://), hostname and port.
type FullyQualifiedHost = String

data HTTPRequest = HTTPRequest { requestOperationId :: Maybe String
                               , requestHost        :: Maybe FullyQualifiedHost
                               , requestMethod      :: Method
                               , requestPath        :: BS.ByteString
                               , requestHeaders     :: RequestHeaders
                               , requestBody        :: Maybe LBS.ByteString }
                                  deriving (Show, Eq)

-- |Given a swagger.json (encoded as a bytestring), decode it, and produce a
--  random Request that complies with the schema.
--  The return type is Either a parsing error (described as String), or a
--  random Request (in the IO monad because it's random).
generateRequestFromJsonDefinition :: BS.ByteString -> Either String (IO HTTPRequest)
generateRequestFromJsonDefinition = fmap generateRequest . eitherDecodeStrict

-- Generate a random request for a Swagger definition
generateRequest :: Swagger -> IO HTTPRequest
generateRequest = generate . requestGenerator

-- Random Request generator
requestGenerator :: Swagger -> Gen HTTPRequest
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

                        let resolveRefSchema :: Referenced Schema -> Maybe Schema
                            resolveRefSchema (Inline p) = Just p
                            resolveRefSchema (Ref r)    = M.lookup (getReference r) $ s ^. definitions

                        -- combine parameters common to all operations to parameters
                        -- specific to the selected operation
                        let params = catMaybes $ resolveRefParam <$> (item ^. parameters) <> (operation ^. parameters)
                            -- partition between required and non-required parameters
                            (requiredParams, notRequiredParams) = partition paramIsRequired params

                        selectedOptionalParams <- sublistOf notRequiredParams

                        -- final list of parameters that must be applied
                        let finalParams = requiredParams <> selectedOptionalParams

                        -- pick params for path
                        let pathParams = catMaybes (paramSchemaAndAllowEmpty ParamPath <$> finalParams)
                        path' <- applyPathTemplating pathParams $ T.pack path

                        -- pick params for query string
                        let queryParams = catMaybes $ paramSchemaAndAllowEmpty ParamQuery <$> finalParams

                        queryStr <- genQuery queryParams

                        let pathAndQuery = C.pack (joinPath [baseP, T.unpack path'])
                                         <> renderQuery True queryStr

                        -- pick params for header
                        let headerParams = catMaybes (paramSchemaAndAllowEmpty ParamHeader <$> finalParams)

                        headers <- genQuery headerParams

                        -- pick params for form data
                        let formDataParams = catMaybes $ paramSchemaAndAllowEmpty ParamFormData <$> finalParams

                        maybeMimeAndBody <- if null formDataParams
                                             then do -- pick a param for body
                                                     bodySchema <- maybeElements $ catMaybes $ (resolveRefSchema =<<) . bodySchemaParam <$> finalParams
                                                     randomJsonBody <- maybe (pure Nothing) (Just <$>) $ genJSON <$> bodySchema
                                                     pure $ (("application/json",) . encode) <$> randomJsonBody
                                             else do formDataQuery <- genQuery formDataParams
                                                     pure $ Just ( "application/x-www-form-urlencoded"
                                                                 , toLazyByteString $ renderQueryBuilder False formDataQuery)

                        let headers' = catMaybes
                                     $   (\h -> (fst h,) <$> snd h)
                                     <$> ((mk . fst &&& snd) <$> headers)
                                     <>  [(hHost, (C.pack . (^. name)) <$> mHost)]
                                     <>  [(hContentType, fst <$> maybeMimeAndBody)]

                        -- use scheme from operation, if defined, or from global
                        scheme <- elements $ fromMaybe [Https] (operation ^. schemes <|> s ^. schemes)
                        pure $ HTTPRequest (T.unpack <$> operation ^. operationId)
                                           (buildHost scheme <$> mHost)
                                           method
                                           pathAndQuery
                                           headers'
                                           (snd <$> maybeMimeAndBody)

    where
      buildHost :: Scheme -> Host -> String
      buildHost sc h = schemeToHttpPrefix sc <> (h ^. name) <> maybe "" ((':':) . show) (h ^. port)

      schemeToHttpPrefix Http  = "http://"
      schemeToHttpPrefix Https = "https://"
      schemeToHttpPrefix Ws    = "ws://"
      schemeToHttpPrefix Wss   = "wss://"

      bodySchemaParam :: Param -> Maybe (Referenced Schema)
      bodySchemaParam Param { _paramSchema = ParamBody r} = Just r
      bodySchemaParam _                                   = Nothing

      applyPathTemplating :: [(T.Text, ParamSchema SwaggerKindParamOtherSchema, Bool)] -> T.Text -> Gen T.Text
      applyPathTemplating [] p                 = pure p
      applyPathTemplating ((name, sc, ae):ts) p =
        do v <- paramGen sc ae
           applyPathTemplating ts $ T.replace p ("{" <> name <> "}") v

      genQuery :: [(T.Text, ParamSchema SwaggerKindParamOtherSchema, Bool)] -> Gen Query
      genQuery []                  = pure []
      genQuery ((name, sc, ae):ts) =
        do v <- (\p -> if T.null p then Nothing else Just p) <$> paramGen sc ae
           ((encodeUtf8 name, encodeUtf8 <$> v):) <$> genQuery ts

      paramSchemaAndAllowEmpty :: ParamLocation -> Param -> Maybe (T.Text, ParamSchema SwaggerKindParamOtherSchema, Bool)
      paramSchemaAndAllowEmpty loc Param { _paramName = n, _paramSchema = ParamOther pos@ParamOtherSchema {} }
          | loc == pos ^. in_ = Just ( n
                                     , pos ^. paramSchema
                                     , (loc == ParamQuery || loc == ParamFormData) && fromMaybe False (pos ^. allowEmptyValue))
          | otherwise = Nothing

-- |Useful combinator for (Gen a) family: chose one of the values or
-- Nothing if the list is empty. (i.e. safe "elements")
maybeElements :: [a] -> Gen (Maybe a)
maybeElements [] = pure Nothing
maybeElements xs = (Just . (xs !!)) <$> choose (0, length xs - 1)

paramIsRequired :: Param -> Bool
paramIsRequired Param { _paramSchema = ParamOther ParamOtherSchema { _paramOtherSchemaIn = ParamPath}} = True
paramIsRequired p = fromMaybe False $ p ^. required

-- |Generator for a parameter, which is used on the "path", "query", "form", or
-- "header".
paramGen :: ParamSchema SwaggerKindParamOtherSchema -> Bool -> Gen T.Text
paramGen sc allowEmpty = _

-- |Generate a JSON from a schema
genJSON :: Schema -> Gen Value
genJSON s = _
