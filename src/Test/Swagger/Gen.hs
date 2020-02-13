{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-|
Module      : Test.Swagger.Gen
Description : Exposes a function to generate a random request
Copyright   : (c) Rodrigo Setti, 2017
License     : BSD3
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX

Exposes 'generateRequest', which creates a random request from a Swagger
schema.
-}
module Test.Swagger.Gen (generateRequest) where

import           Control.Applicative        ((<|>))
import           Control.Arrow              ((&&&))
import           Control.Lens               hiding (elements)
import           Control.Monad
import           Data.Aeson
import           Data.Binary.Builder
import           Data.CaseInsensitive
import qualified Data.HashMap.Lazy          as HM
import qualified Data.HashMap.Strict.InsOrd as M
import           Data.List                  (partition)
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Scientific
import           Data.Swagger hiding (version)
import           Data.Swagger.Internal      (SwaggerKind (..))
import qualified Data.Text                  as T
import Data.Text.Encoding
import qualified Data.Vector                as V
import           Network.HTTP.Types
import           System.FilePath.Posix      ((</>))
import           Test.QuickCheck            hiding (Fixed)
import           Test.QuickCheck.Gen        (unGen)
import           Test.QuickCheck.StringRandom (matchRegexp)
import           Test.QuickCheck.Random
import           Test.Swagger.Types
import           Paths_swagger_test      (version)
import           Data.Version            (showVersion)

-- Restrictions on randomly generated value
data ValueRestrictions = ValueRestrictions { _allowEmpty :: Bool, _allowNewline :: Bool }

-- |Given a swagger.json schema, produce a Request that complies with the schema.
--  The return type is a random Request (in the IO monad because it's random).
generateRequest :: Seed -> Size -> NormalizedSwagger -> Maybe OperationId -> (Operation, HttpRequest)
generateRequest seed size model mopid =
  let gen = mkQCGen seed
   in unGen (requestGenerator model mopid) gen size

-- Random Request generator
requestGenerator :: NormalizedSwagger -> Maybe OperationId -> Gen (Operation, HttpRequest)
requestGenerator ns mopid =
 do let s = getSwagger ns
        baseP = fromMaybe "/" $ s ^. basePath
        mHost = s ^. host

    -- compute all available operations, in a 4-tuple
    let availableOps :: [(FilePath, PathItem, Method, Operation)]
        availableOps = catMaybes $ mconcat $
          (\(path, item) ->
              [  (path, item, methodGet,) <$> item ^. get
               , (path, item, methodPut,) <$> item ^. put
               , (path, item, methodPost,) <$> item ^. post
               , (path, item, methodDelete,) <$> item ^. delete
               , (path, item, methodOptions,) <$> item ^. options
               , (path, item, methodHead,) <$> item ^. head_
               , (path, item, methodPatch,) <$> item ^. patch ])
          <$> M.toList (s ^. paths)

    -- select one operation of the selected path either randomly or lookup by
    -- operation id if providede
    (path, item, method, operation) <-
      case mopid of
        Nothing -> elements availableOps
        Just opid ->
          do let opId2Op = catMaybes
                         $ (\i -> let (_, _, _, o) = i in (,i) <$> o ^. operationId)
                         <$> availableOps
                 found = lookup opid opId2Op
                 allIds = T.intercalate ", " $ fst <$> opId2Op
             maybe (fail $ "undefined operation id: \""
                         <> T.unpack opid
                         <> "\". Available ids: "
                         <> T.unpack allIds)
                    pure found

    -- combine parameters common to all operations to parameters
    -- specific to the selected operation
    let params = catMaybes $ refToMaybe <$> (item ^. parameters) <> (operation ^. parameters)
        -- partition between required and non-required parameters
        (requiredParams, notRequiredParams) = partition paramIsRequired params

    selectedOptionalParams <- sublistOf notRequiredParams

    -- final list of parameters that must be applied
    let finalParams = requiredParams <> selectedOptionalParams

    -- pick params for path
    let pathParams = catMaybes (paramSchemaAndValueRestrictions ParamPath <$> finalParams)
    path' <- applyPathTemplating pathParams $ T.pack path

    -- pick params for query string
    let queryParams = catMaybes $ paramSchemaAndValueRestrictions ParamQuery <$> finalParams

    queryStr <- genQuery queryParams

    -- pick params for header
    let headerParams = catMaybes (paramSchemaAndValueRestrictions ParamHeader <$> finalParams)

    randomHeaders <- genQuery headerParams

    -- pick params for form data
    let formDataParams = catMaybes $ paramSchemaAndValueRestrictions ParamFormData <$> finalParams

    maybeMimeAndBody <-
      if null formDataParams
       then do -- pick a param for body
               bodySchema <- maybeElements $ catMaybes $ (refToMaybe =<<) . bodySchemaParam <$> finalParams
               randomJsonBody <- maybe (pure Nothing) (Just <$>) $ genJSON <$> bodySchema
               pure $ (("application/json",) . encode) <$> randomJsonBody
       else do formDataQuery <- genQuery formDataParams
               pure $ Just ( "application/x-www-form-urlencoded"
                           , toLazyByteString $ renderQueryText False formDataQuery)

    let randomHeaders' = catMaybes
                 $   (\h -> (fst h,) <$> snd h)
                 <$> ((mk . fst &&& snd) <$> randomHeaders)
                 <>  [("Host", (T.pack . hostNameAndPort) <$> mHost)]
                 <>  [("Content-Type", fst <$> maybeMimeAndBody)]
                 <>  [("User-Agent", Just $ "swagger-test/" <> T.pack (showVersion version))]

    -- use scheme from operation, if defined, or from global
    scheme <- elements $ fromMaybe [schemeForPort $ view port =<< mHost] (operation ^. schemes <|> s ^. schemes)
    pure ( operation
          , HttpRequest (buildHost scheme <$> mHost)
                        method
                        (T.pack (baseP </> T.unpack path'))
                        queryStr
                        randomHeaders'
                        (snd <$> maybeMimeAndBody) )

 where
  schemeForPort (Just 80) = Http
  schemeForPort (Just 443) = Https
  schemeForPort _ = Http

  buildHost :: Scheme -> Host -> String
  buildHost sc h = schemeToHttpPrefix sc <> hostNameAndPort h

  hostNameAndPort :: Host -> String
  hostNameAndPort h = (h ^. name) <> maybe "" ((':':) . show) (h ^. port)

  schemeToHttpPrefix Http  = "http://"
  schemeToHttpPrefix Https = "https://"
  schemeToHttpPrefix Ws    = "ws://"
  schemeToHttpPrefix Wss   = "wss://"

  bodySchemaParam :: Param -> Maybe (Referenced Schema)
  bodySchemaParam Param { _paramSchema = ParamBody r} = Just r
  bodySchemaParam _                                   = Nothing

  applyPathTemplating :: [(T.Text, ParamSchema k, ValueRestrictions)] -> T.Text -> Gen T.Text
  applyPathTemplating [] p                 = pure p
  applyPathTemplating ((key, sc, res):ts) p =
    do let f = sc ^. format
       v <- (mconcat . jsonToText f CollectionSSV) <$> paramGen sc res
       applyPathTemplating ts $ T.replace ("{" <> key <> "}") (urlEncodeText v) p

  genQuery :: [(T.Text, ParamSchema k, ValueRestrictions)] -> Gen QueryText
  genQuery []                  = pure []
  genQuery ((key, sc, res):ts) =
    do let f = sc ^. format
       v <- jsonToText f CollectionCSV <$> paramGen sc res
       let this = (\x -> (key, if T.null x then Nothing else Just x)) <$> v
       rest <- genQuery ts
       pure $ this <> rest

  urlEncodeText :: T.Text -> T.Text
  urlEncodeText = decodeUtf8 . urlEncode False . encodeUtf8

  paramSchemaAndValueRestrictions :: ParamLocation -> Param -> Maybe (T.Text, ParamSchema 'SwaggerKindParamOtherSchema, ValueRestrictions)
  paramSchemaAndValueRestrictions loc Param { _paramName = n, _paramSchema = ParamOther pos@ParamOtherSchema {} }
      | loc == pos ^. in_ = Just ( n
                                 , pos ^. paramSchema
                                 , ValueRestrictions
                                     { _allowEmpty = (loc == ParamQuery || loc == ParamFormData)
                                                    && fromMaybe False (pos ^. allowEmptyValue)
                                     , _allowNewline = loc /= ParamHeader
                                     }
                                 )
      | otherwise = Nothing
  paramSchemaAndValueRestrictions _ Param { _paramSchema = ParamBody _ } = Nothing

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
paramGen :: ParamSchema a -> ValueRestrictions -> Gen Value
paramGen ParamSchema { _paramSchemaEnum=Just values} (ValueRestrictions allowEmpty _) = elements $ values <> [Null | allowEmpty]
paramGen ParamSchema { _paramSchemaType=SwaggerString, _paramSchemaPattern=Just pat } _ = toJSON <$> matchRegexp pat
paramGen ParamSchema { _paramSchemaType=SwaggerString } res = genJString res

-- TODO: respect "multiple of" number generation
paramGen ps@ParamSchema { _paramSchemaType=SwaggerNumber } (ValueRestrictions allowEmpty _) =
  do let n :: Gen Double
         min_ = fromMaybe (-1/0) $ toRealFloat <$> ps ^. minimum_
         max_ = fromMaybe (1/0) $ toRealFloat <$> ps ^. maximum_
         n = choose (min_, max_)
     frequency $ [(10, Number . fromFloatDigits <$> n)] <> [(1, pure Null) | allowEmpty]
paramGen ps@ParamSchema { _paramSchemaType=SwaggerInteger } (ValueRestrictions allowEmpty _) =
  do let n :: Gen Int
         min_ = fromMaybe (-1000) $ toBoundedInteger =<< ps ^. minimum_
         max_ = fromMaybe 1000 $ toBoundedInteger =<< ps ^. maximum_
         n = choose ( min_ + if fromMaybe False $ ps ^. exclusiveMinimum then 1 else 0
                     , max_ - if fromMaybe False $ ps ^. exclusiveMaximum then 1 else 0)
     frequency $ [(10, Number . fromInteger . toInteger <$> n)] <> [(1, pure Null) | allowEmpty]
paramGen ParamSchema { _paramSchemaType=SwaggerBoolean } (ValueRestrictions allowEmpty _) =
  elements $ [Bool True, Bool False] <> [Null | allowEmpty]

-- TODO: respect generation of "unique items"
paramGen ps@ParamSchema { _paramSchemaType=SwaggerArray, _paramSchemaFormat=fmt } res =
  do  siz <- toInteger <$> getSize
      len <- fromIntegral <$> choose ( fromMaybe (if _allowEmpty res then 0 else 1) $ ps ^. minLength
                                      , fromMaybe siz $ ps ^. maxLength)
      case ps ^. items of
        Just (SwaggerItemsObject (Inline s)) ->
          toJSON <$> replicateM len (genJSON s)
        Just (SwaggerItemsArray rs) ->
          toJSON <$> mapM genJSON (catMaybes (refToMaybe <$> rs))
        Just (SwaggerItemsPrimitive cfmt ps') ->
           do x <- toJSON <$> replicateM len (paramGen ps' res)
              pure $ maybe x (toJSON . flip (jsonToText fmt) x) cfmt
        _ ->
          toJSON <$> replicateM len (genJString res)

-- NOTE: we don't really support files
paramGen ParamSchema { _paramSchemaType=SwaggerFile } res = genJString res
paramGen ParamSchema { _paramSchemaType=SwaggerNull } _ = pure Null

-- TODO: what to do here?
paramGen ParamSchema { _paramSchemaType=SwaggerObject } _ = undefined

jsonToText :: Maybe Format -> CollectionFormat t -> Value -> [T.Text]
jsonToText _ _ (String t) = [t]
jsonToText _ _ Null = []
jsonToText _ _ (Bool True) = ["true"]
jsonToText _ _ (Bool False) = ["false"]
jsonToText f _ (Number n) = [T.pack $ display n]
  where
    display = case f of
                Just "double" -> formatScientific Fixed Nothing
                Just "float"  -> formatScientific Fixed Nothing
                _             -> formatScientific Fixed (Just 0)

jsonToText fmt cfmt (Object m) =
  let txts = concatMap (\i -> (\x -> fst i <> "=" <> x) <$> jsonToText fmt cfmt (snd i)) $ HM.toList m
  in case cfmt of
    CollectionCSV   -> [T.intercalate "," txts]
    CollectionSSV   -> [T.intercalate " " txts]
    CollectionTSV   -> [T.intercalate "\t" txts]
    CollectionPipes -> [T.intercalate "|" txts]
    CollectionMulti -> txts
jsonToText fmt cfmt (Array v) =
  let txts = concatMap (jsonToText fmt cfmt) $ V.toList v
  in case cfmt of
    CollectionCSV   -> [T.intercalate "," txts]
    CollectionSSV   -> [T.intercalate " " txts]
    CollectionTSV   -> [T.intercalate "\t" txts]
    CollectionPipes -> [T.intercalate "|" txts]
    CollectionMulti -> txts

-- |Merge two Json values, if possible
merge :: Value -> Value -> Value
merge Null v                  = v
merge v Null                  = v
merge (Array v1) (Array v2)   = Array $ v1 <> v2
merge (Object v1) (Object v2) = Object $ v1 <> v2
merge v _                     = v

-- |Generate a JSON from a schema
genJSON :: Schema -> Gen Value
genJSON Schema { _schemaAllOf = Just ss } =
  let ss' = catMaybes $ refToMaybe <$> ss
  in foldl merge Null <$> mapM genJSON ss'

genJSON s@Schema { _schemaParamSchema = ParamSchema { _paramSchemaType = SwaggerObject } } =
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
                    Just (AdditionalPropertiesSchema (Inline s')) -> replicateM (nProps - (nOptProps + length reqProps)) $
                                            do k <- genNonemptyText
                                               (k,) <$> genJSON s'
                    _ -> pure []

     pure $ Object $ HM.fromList $ reqPropsV <> optPropsV <> addPropsV

genJSON Schema { _schemaParamSchema = ps } = paramGen ps (ValueRestrictions True True)

genNonemptyText :: Gen T.Text
genNonemptyText = genText (ValueRestrictions False True)

genText :: ValueRestrictions -> Gen T.Text
genText (ValueRestrictions allowEmpty allowNewline) =
  do c <- arbitraryChar
     s <- arbitraryString
     pure $ T.pack $ [c | not allowEmpty] <> s
 where
  arbitraryChar
    | allowNewline = arbitraryASCIIChar
    | otherwise = arbitraryASCIIChar `suchThat` (not . (=='\n'))
  arbitraryString = listOf arbitraryChar

genJString :: ValueRestrictions -> Gen Value
genJString res = toJSON <$> genText res
