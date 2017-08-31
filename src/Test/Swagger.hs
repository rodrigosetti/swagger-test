module Test.Swagger (resolveReferences, refToMaybe) where

import           Control.Lens
import           Data.Generics
import qualified Data.HashMap.Strict.InsOrd as M
import           Data.Monoid                ((<>))
import           Data.Swagger
import qualified Data.Text                  as T

-- |Replace all references with inlines
resolveReferences :: Swagger -> Swagger
resolveReferences s = everywhere' (mkT resolveSchema) $ everywhere' (mkT resolveParam) s
  -- NOTE: we need to use the top-down everywhere variant for this to work as intented
  where
    resolveParam :: Referenced Param -> Referenced Param
    resolveParam i@Inline {} = i
    resolveParam (Ref (Reference r))  = maybe (error $ "undefied schema: " <> T.unpack r) Inline
                                      $ M.lookup r $ s ^. parameters
    resolveSchema :: Referenced Schema -> Referenced Schema
    resolveSchema i@Inline {} = i
    resolveSchema (Ref (Reference r)) = maybe (error $ "undefied schema: " <> T.unpack r) Inline
                                      $ M.lookup r $ s ^. definitions

-- |Transform a reference into a Just value if is inline, Nothing, otherwise
refToMaybe :: Referenced a -> Maybe a
refToMaybe (Inline i) = Just i
refToMaybe (Ref _)    = Nothing
