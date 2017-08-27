{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes     #-}
module Test.Swagger.Gen ( HTTPRequest(..)
                        , generateRequestFromJsonDefinition) where

import           Control.Applicative        ((<|>))
import           Control.Arrow              ((&&&))
import           Control.Lens               hiding (elements)
import           Control.Monad
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
import           Data.Scientific
import           Data.Swagger
import           Data.Swagger.Internal      (SwaggerKind (..))
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Network.HTTP.Types
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.URI
import           System.FilePath.Posix      (joinPath)
import           Test.QuickCheck
import           Data.Generics
import qualified Data.HashMap.Lazy  as HM
import qualified Data.Vector as V

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

-- |Replace all references with inlines
resolveReferences :: Swagger -> Swagger
resolveReferences s = everywhere (mkT resolveSchema) $ everywhere (mkT resolveParam) s
  where
    resolveParam :: Referenced Param -> Referenced Param
    resolveParam i@Inline {} = i
    resolveParam (Ref (Reference r))  = maybe (error $ "undefied schema: " <> T.unpack r) Inline
                                      $ M.lookup r $ s ^. parameters
    resolveSchema :: Referenced Schema -> Referenced Schema
    resolveSchema i@Inline {} = i
    resolveSchema (Ref (Reference r)) = maybe (error $ "undefied schema: " <> T.unpack r) Inline
                                      $ M.lookup r $ s ^. definitions

refToMaybe :: Referenced a -> Maybe a
refToMaybe (Inline i) = Just i
refToMaybe (Ref _) = Nothing

-- Random Request generator
requestGenerator :: Swagger -> Gen HTTPRequest
requestGenerator s' =
 do let s = resolveReferences s'
        baseP = fromMaybe "/" $ s ^. basePath
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


    -- combine parameters common to all operations to parameters
    -- specific to the selected operation
    let params = catMaybes $ refToMaybe <$> (item ^. parameters) <> (operation ^. parameters)
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

    maybeMimeAndBody <-
      if null formDataParams
       then do -- pick a param for body
               bodySchema <- maybeElements $ catMaybes $ (refToMaybe =<<) . bodySchemaParam <$> finalParams
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
    do v <- jsonToText CollectionSSV <$> paramGen sc ae
       applyPathTemplating ts $ T.replace p ("{" <> name <> "}") v

  genQuery :: [(T.Text, ParamSchema SwaggerKindParamOtherSchema, Bool)] -> Gen Query
  genQuery []                  = pure []
  genQuery ((name, sc, ae):ts) =
    do v <- (\p -> if T.null p then Nothing else Just p) . jsonToText CollectionCSV <$> paramGen sc ae
       ((encodeUtf8 name, encodeUtf8 <$> v):) <$> genQuery ts

  paramSchemaAndAllowEmpty :: ParamLocation -> Param -> Maybe (T.Text, ParamSchema SwaggerKindParamOtherSchema, Bool)
  paramSchemaAndAllowEmpty loc Param { _paramName = n, _paramSchema = ParamOther pos@ParamOtherSchema {} }
      | loc == pos ^. in_ = Just ( n
                                 , pos ^. paramSchema
                                 , (loc == ParamQuery || loc == ParamFormData)
                                   && fromMaybe False (pos ^. allowEmptyValue))
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
-- TODO: respect "pattern" generation
paramGen :: ParamSchema a -> Bool -> Gen Value
paramGen ParamSchema { _paramSchemaEnum=Just values} allowEmpty = elements $ values <> [Null | allowEmpty]
paramGen ParamSchema { _paramSchemaType=SwaggerString } allowEmpty = genJString allowEmpty

-- TODO: respect "multiple of" number generation
paramGen ps@ParamSchema { _paramSchemaType=SwaggerNumber } allowEmpty =
  do let n :: Gen Double
         min_ = fromMaybe (-1/0) $ toRealFloat <$> ps ^. minimum_
         max_ = fromMaybe (1/0) $ toRealFloat <$> ps ^. maximum_
         n = choose (min_, max_)
     frequency $ [(10, Number . fromFloatDigits <$> n)] <> [(1, pure Null) | allowEmpty]
paramGen ps@ParamSchema { _paramSchemaType=SwaggerInteger } allowEmpty =
  do let n :: Gen Int
         min_ = fromMaybe minBound $ toBoundedInteger =<< ps ^. minimum_
         max_ = fromMaybe maxBound $ toBoundedInteger =<< ps ^. maximum_
         n = choose ( min_ + if fromMaybe False $ ps ^. exclusiveMinimum then 1 else 0
                     , max_ - if fromMaybe False $ ps ^. exclusiveMaximum then 1 else 0)
     frequency $ [(10, Number . fromInteger . toInteger <$> n)] <> [(1, pure Null) | allowEmpty]
paramGen ps@ParamSchema { _paramSchemaType=SwaggerBoolean } allowEmpty =
  elements $ [Bool True, Bool False] <> [Null | allowEmpty]

-- TODO: respect generation of "unique items"
paramGen ps@ParamSchema { _paramSchemaType=SwaggerArray } allowEmpty =
  do  siz <- toInteger <$> getSize
      len <- fromIntegral <$> choose ( fromMaybe (if allowEmpty then 0 else 1) $ ps ^. minLength
                                      , fromMaybe siz $ ps ^. maxLength)
      case ps ^. items of
        Nothing ->
          toJSON <$> replicateM len (genJString allowEmpty)
        Just (SwaggerItemsObject (Inline s)) ->
          toJSON <$> replicateM len (genJSON s)
        Just (SwaggerItemsArray rs) ->
          toJSON <$> mapM genJSON (catMaybes (refToMaybe <$> rs))
        Just (SwaggerItemsPrimitive fmt ps') ->
           do x <- toJSON <$> replicateM len (paramGen ps' allowEmpty)
              pure $ maybe x (toJSON . flip jsonToText x) fmt

-- NOTE: we don't really support files
paramGen ps@ParamSchema { _paramSchemaType=SwaggerFile } allowEmpty = genJString allowEmpty
paramGen ps@ParamSchema { _paramSchemaType=s@SwaggerObject } allowEmpty = _
paramGen    ParamSchema { _paramSchemaType=SwaggerNull } _ = pure Null

jsonToText :: CollectionFormat t -> Value -> T.Text
jsonToText _ (String t) = t
jsonToText _ Null = ""
jsonToText _ (Bool True) = "true"
jsonToText _ (Bool False) = "false"
jsonToText _ (Number n) = T.pack $ show n
jsonToText fmt (Object m) = T.intercalate (collectionSep fmt) $ (\i -> fst i <> "=" <> jsonToText fmt (snd i)) <$> HM.toList m
jsonToText fmt (Array v) = T.intercalate (collectionSep fmt) $ jsonToText fmt <$> V.toList v

collectionSep :: CollectionFormat t -> T.Text
collectionSep CollectionCSV   = ","
collectionSep CollectionSSV   = ";"
collectionSep CollectionTSV   = "\t"
collectionSep CollectionPipes = "|"
collectionSep CollectionMulti = "" -- NOTE: what is this?

-- |Generate a JSON from a schema
genJSON :: Schema -> Gen Value
-- TODO: what is "all of" exactly?
genJSON Schema { _schemaAllOf = Just ss } | not (null ss) = oneof $ genJSON <$> ss
genJSON s =
  do let props = catMaybes $ (\i -> (fst i,) <$> refToMaybe (snd i)) <$> M.toList (s ^. properties)
         (reqProps, optProps) = partition (\i -> fst i `elem` s ^. required) props
     siz <- toInteger <$> getSize
     nProps <- fromIntegral <$> choose ( fromMaybe 0 $ s ^. minProperties
                                       , fromMaybe siz $ s ^. maxProperties)
     nOptProps <- choose (0, nProps)
     decidedOptProps <- take nOptProps <$> shuffle optProps

     reqPropsV <- mapM (\i -> (fst i,) <$> genJSON (snd i)) reqProps
     optPropsV <- mapM (\i -> (fst i,) <$> genJSON (snd i)) decidedOptProps
     addPropsV <- case s ^. additionalProperties of
                    Just (Inline s') -> replicateM (nProps - (nOptProps + length reqProps)) $
                                            do k <- genNonemptyText
                                               (k,) <$> genJSON s'
                    _ -> pure []

     pure $ Object $ HM.fromList $ reqPropsV <> optPropsV <> addPropsV

genNonemptyText :: Gen T.Text
genNonemptyText = genText False

genText :: Bool -> Gen T.Text
genText allowEmpty =
  do c <- arbitraryASCIIChar
     s <- getASCIIString <$> arbitrary
     pure $ T.pack $ [c | not allowEmpty] <> s

genJString :: Bool -> Gen Value
genJString allowEmpty = toJSON <$> genText allowEmpty
