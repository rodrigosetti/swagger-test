{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
module Test.Swagger.Gen ( module Test.Swagger.Types
                        , generateRequest) where

import           Control.Applicative        ((<|>))
import           Control.Arrow              ((&&&))
import           Control.Lens               hiding (elements)
import           Control.Monad
import           Data.Aeson
import           Data.Binary.Builder
import           Data.CaseInsensitive
import           Data.Generics
import qualified Data.HashMap.Lazy          as HM
import qualified Data.HashMap.Strict.InsOrd as M
import           Data.List                  (partition)
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Scientific
import           Data.Swagger
import           Data.Swagger.Internal      (SwaggerKind (..))
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Network.HTTP.Types
import           System.FilePath.Posix      (joinPath)
import           Test.QuickCheck            hiding (Fixed)
import           Test.QuickCheck.Gen        (unGen)
import           Test.QuickCheck.Random
import           Test.Swagger.Types


-- |Given a swagger.json schema, produce a Request that complies with the schema.
--  The return type is a random Request (in the IO monad because it's random).
generateRequest :: Seed -> Swagger -> Maybe OperationId -> HTTPRequest
generateRequest seed s mopid =
  let gen = mkQCGen seed
   in unGen (requestGenerator s mopid) gen 30

-- |Replace all references with inlines
resolveReferences :: Swagger -> Swagger
resolveReferences s = everywhere' (mkT resolveSchema) $ everywhere' (mkT resolveParam) s
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
refToMaybe (Ref _)    = Nothing

-- Random Request generator
requestGenerator :: Swagger -> Maybe OperationId -> Gen HTTPRequest
requestGenerator s' mopid =
 do let s = resolveReferences s'
        baseP = fromMaybe "/" $ s ^. basePath
        mHost = s ^. host

    -- compute all available operations, in a 4-tuple
    let availableOps :: [(FilePath, PathItem, Method, Operation)]
        availableOps = catMaybes $ mconcat $
          (\i -> let (path, item) = i
                 in [  (path, item, methodGet,) <$> item ^. get
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
    let pathParams = catMaybes (paramSchemaAndAllowEmpty ParamPath <$> finalParams)
    path' <- applyPathTemplating pathParams $ T.pack path

    -- pick params for query string
    let queryParams = catMaybes $ paramSchemaAndAllowEmpty ParamQuery <$> finalParams

    queryStr <- genQuery queryParams

    -- pick params for header
    let headerParams = catMaybes (paramSchemaAndAllowEmpty ParamHeader <$> finalParams)

    randomHeaders <- genQuery headerParams

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
                           , toLazyByteString $ renderQueryText False formDataQuery)

    let randomHeaders' = catMaybes
                 $   (\h -> (fst h,) <$> snd h)
                 <$> ((mk . fst &&& snd) <$> randomHeaders)
                 <>  [("Host", (T.pack . (^. name)) <$> mHost)]
                 <>  [("Content-Type", fst <$> maybeMimeAndBody)]

    -- use scheme from operation, if defined, or from global
    scheme <- elements $ fromMaybe [Https] (operation ^. schemes <|> s ^. schemes)
    pure $ HTTPRequest (operation ^. operationId)
                       (buildHost scheme <$> mHost)
                       method
                       (T.pack (joinPath [baseP, T.unpack path']))
                       queryStr
                       randomHeaders'
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

  applyPathTemplating :: [(T.Text, ParamSchema k, Bool)] -> T.Text -> Gen T.Text
  applyPathTemplating [] p                 = pure p
  applyPathTemplating ((key, sc, ae):ts) p =
    do let f = sc ^. format
       v <- (mconcat . jsonToText f CollectionSSV) <$> paramGen sc ae
       applyPathTemplating ts $ T.replace ("{" <> key <> "}") v p

  genQuery :: [(T.Text, ParamSchema k, Bool)] -> Gen QueryText
  genQuery []                  = pure []
  genQuery ((key, sc, ae):ts) =
    do let f = sc ^. format
       v <- jsonToText f CollectionCSV <$> paramGen sc ae
       let this = (\x -> (key, if T.null x then Nothing else Just x)) <$> v
       rest <- genQuery ts
       pure $ this <> rest

  paramSchemaAndAllowEmpty :: ParamLocation -> Param -> Maybe (T.Text, ParamSchema 'SwaggerKindParamOtherSchema, Bool)
  paramSchemaAndAllowEmpty loc Param { _paramName = n, _paramSchema = ParamOther pos@ParamOtherSchema {} }
      | loc == pos ^. in_ = Just ( n
                                 , pos ^. paramSchema
                                 , (loc == ParamQuery || loc == ParamFormData)
                                   && fromMaybe False (pos ^. allowEmptyValue))
      | otherwise = Nothing
  paramSchemaAndAllowEmpty _ Param { _paramSchema = ParamBody _ } = Nothing

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
         min_ = fromMaybe (-1000) $ toBoundedInteger =<< ps ^. minimum_
         max_ = fromMaybe 1000 $ toBoundedInteger =<< ps ^. maximum_
         n = choose ( min_ + if fromMaybe False $ ps ^. exclusiveMinimum then 1 else 0
                     , max_ - if fromMaybe False $ ps ^. exclusiveMaximum then 1 else 0)
     frequency $ [(10, Number . fromInteger . toInteger <$> n)] <> [(1, pure Null) | allowEmpty]
paramGen ParamSchema { _paramSchemaType=SwaggerBoolean } allowEmpty =
  elements $ [Bool True, Bool False] <> [Null | allowEmpty]

-- TODO: respect generation of "unique items"
paramGen ps@ParamSchema { _paramSchemaType=SwaggerArray, _paramSchemaFormat=fmt } allowEmpty =
  do  siz <- toInteger <$> getSize
      len <- fromIntegral <$> choose ( fromMaybe (if allowEmpty then 0 else 1) $ ps ^. minLength
                                      , fromMaybe siz $ ps ^. maxLength)
      case ps ^. items of
        Just (SwaggerItemsObject (Inline s)) ->
          toJSON <$> replicateM len (genJSON s)
        Just (SwaggerItemsArray rs) ->
          toJSON <$> mapM genJSON (catMaybes (refToMaybe <$> rs))
        Just (SwaggerItemsPrimitive cfmt ps') ->
           do x <- toJSON <$> replicateM len (paramGen ps' allowEmpty)
              pure $ maybe x (toJSON . flip (jsonToText fmt) x) cfmt
        _ ->
          toJSON <$> replicateM len (genJString allowEmpty)

-- NOTE: we don't really support files
paramGen ParamSchema { _paramSchemaType=SwaggerFile } allowEmpty = genJString allowEmpty
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
genJSON Schema { _schemaAllOf = Just ss } | not (null ss) =
  do jsons <- shuffle =<< mapM genJSON ss
     n <- choose (1, length jsons)
     pure $ foldl1 merge $ take n jsons

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
                    Just (Inline s') -> replicateM (nProps - (nOptProps + length reqProps)) $
                                            do k <- genNonemptyText
                                               (k,) <$> genJSON s'
                    _ -> pure []

     pure $ Object $ HM.fromList $ reqPropsV <> optPropsV <> addPropsV

genJSON Schema { _schemaParamSchema = ps } = paramGen ps True

genNonemptyText :: Gen T.Text
genNonemptyText = genText False

genText :: Bool -> Gen T.Text
genText allowEmpty =
  do c <- arbitraryASCIIChar
     s <- getASCIIString <$> arbitrary
     pure $ T.pack $ [c | not allowEmpty] <> s

genJString :: Bool -> Gen Value
genJString allowEmpty = toJSON <$> genText allowEmpty
