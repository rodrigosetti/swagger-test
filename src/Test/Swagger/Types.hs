module Test.Swagger.Types where

import qualified Data.ByteString.Lazy as LBS
import           Data.CaseInsensitive
import qualified Data.Text            as T
import           Network.HTTP.Types

-- |The FullyQualifiedHost contains the scheme (i.e. http://), hostname and port.
type FullyQualifiedHost = String

type Seed = Int
type OperationId = T.Text

type HttpHeader = (CI T.Text, T.Text)
type Headers = [HttpHeader]

data HTTPRequest = HTTPRequest { requestOperationId :: Maybe OperationId
                               , requestHost        :: Maybe FullyQualifiedHost
                               , requestMethod      :: Method
                               , requestPath        :: T.Text
                               , requestQuery       :: QueryText
                               , requestHeaders     :: Headers
                               , requestBody        :: Maybe LBS.ByteString }
                      deriving (Show)

data HttpResponse = HTTPResponse { responseHttpVersion :: HttpVersion
                                 , responseStatus      :: Status
                                 , responseHeaders     :: Headers
                                 , responseBody        :: Maybe LBS.ByteString }
                      deriving (Show)
