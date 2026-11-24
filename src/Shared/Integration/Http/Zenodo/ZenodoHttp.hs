module Shared.Integration.Http.Zenodo.ZenodoHttp (
  ZenodoResponse (..),
  zenodoRequest,
  expectStatus,
  expectJson,
  formatZenodoFailure,
  extractZenodoErrorDetail,
) where

import qualified Control.Exception.Base as E
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Client (responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)

import Shared.Integration.Http.Common.HttpClient (runSimpleRequest)
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Http.HttpRequest

data ZenodoResponse = ZenodoResponse
  { status :: Int
  , body :: BSL.ByteString
  }
  deriving (Show, Eq)

-- Sends a request to Zenodo; Left only when no response arrived (the status code is checked by the caller)
zenodoRequest :: WizardRequestContextC s m => String -> String -> String -> M.Map String String -> BS.ByteString -> m (Either String ZenodoResponse)
zenodoRequest operation method url headers reqBody = do
  let request = HttpRequest {requestMethod = method, requestUrl = url, requestHeaders = headers, requestBody = reqBody, multipart = Nothing}
  eResponse <- runSimpleRequest request
  return $
    case eResponse of
      Right response -> Right ZenodoResponse {status = statusCode . responseStatus $ response, body = responseBody response}
      Left exception -> Left (operation ++ " failed: " ++ exceptionName exception ++ ".")
  where
    exceptionName :: E.SomeException -> String
    exceptionName = takeWhile (/= ' ') . show

expectStatus :: String -> [Int] -> ZenodoResponse -> Either String ZenodoResponse
expectStatus operation expected response
  | response.status `elem` expected = Right response
  | otherwise = Left (formatZenodoFailure operation response)

expectJson :: FromJSON a => String -> [Int] -> ZenodoResponse -> Either String a
expectJson operation expected response = do
  _ <- expectStatus operation expected response
  case eitherDecode response.body of
    Right value -> Right value
    Left _ -> Left ("Unexpected response from " ++ operation ++ ".")

-- Port of _format_zenodo_failure of the Zenodo plugin: the operation, the status and the details Zenodo returned
formatZenodoFailure :: String -> ZenodoResponse -> String
formatZenodoFailure operation response =
  case extractZenodoErrorDetail response.body of
    Just detail -> operation ++ " failed (status " ++ show response.status ++ "): " ++ detail ++ "."
    Nothing -> operation ++ " failed (status " ++ show response.status ++ ")."

extractZenodoErrorDetail :: BSL.ByteString -> Maybe String
extractZenodoErrorDetail rawBody =
  case decode rawBody of
    Just value -> detailOf value
    Nothing ->
      let text = unwords . words . BSL.unpack $ rawBody
       in if null text then Nothing else Just (take 300 text)
  where
    detailOf :: Value -> Maybe String
    detailOf (String text) = nonEmpty (unwords . words . T.unpack $ text)
    detailOf (Array values) = joined (fmap detailOf (V.toList values))
    detailOf (Object object) = joined [KM.lookup key object >>= detailOf | key <- detailKeys]
    detailOf _ = Nothing
    detailKeys :: [Key]
    detailKeys = ["error_description", "message", "detail", "error", "reason", "defaultMessage", "errors", "messages"]
    joined details =
      case L.nub [detail | Just detail <- details] of
        [] -> Nothing
        parts -> Just (take 300 (L.intercalate "; " parts))
    nonEmpty "" = Nothing
    nonEmpty text = Just (take 300 text)
