module Main where

import           Control.Monad              (forM_)
import           Data.Aeson                 (encode)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.CaseInsensitive       (original)
import           Data.Maybe                 (fromMaybe)
import           System.Exit                (die)
import           Test.Swagger.Gen

main :: IO ()
main = do contents <- BS.readFile "swagger.json"
          case generateRequestFromJsonDefinition contents of
            Left e  -> die e
            Right r -> printRequest =<< r


printRequest :: JsonHTTPRequest -> IO ()
printRequest (JsonHTTPRequest host method path headers body) =
  do BS.putStr method
     putStr " "
     putStr $ fromMaybe "" host
     putStrLn path
     forM_ headers $ \(k,v) -> BS.putStr (original k) >> putStr ": " >> C.putStrLn v
     case body of
       Just b  -> putStr "\n" >> LC.putStrLn (encode b)
       Nothing -> pure ()
