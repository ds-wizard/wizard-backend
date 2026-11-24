-- The InvenioRDM records API of Zenodo (port of zenodo_client.py of the Zenodo submission plugin)
module Shared.Integration.Http.Zenodo.ZenodoRecords (
  ZenodoClient (..),
  createRecord,
  getDraft,
  getRecord,
  updateDraft,
  createNewVersion,
  deleteDraft,
  listDraftFileKeys,
  deleteDraftFile,
  uploadDraftFile,
  resolveCommunityId,
  attachToCommunity,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Network.HTTP.Types.URI (urlEncode)

import Shared.Integration.Http.Zenodo.ZenodoHttp
import Shared.Model.Context.WizardRequestContext

data ZenodoClient = ZenodoClient
  { baseUrl :: String
  , accessToken :: String
  }

-- POST /api/records
createRecord :: WizardRequestContextC s m => ZenodoClient -> Value -> m (Either String Value)
createRecord client payload = jsonCall client "Zenodo draft creation" "POST" (recordsUrl client []) (Just payload) [201]

-- GET /api/records/{id}/draft; Right Nothing when the draft does not exist (anymore)
getDraft :: WizardRequestContextC s m => ZenodoClient -> String -> m (Either String (Maybe Value))
getDraft client recordId = optionalCall client "Zenodo draft lookup" (recordsUrl client [recordId, "draft"])

-- GET /api/records/{id}; Right Nothing when the record is not published
getRecord :: WizardRequestContextC s m => ZenodoClient -> String -> m (Either String (Maybe Value))
getRecord client recordId = optionalCall client "Zenodo record lookup" (recordsUrl client [recordId])

-- PUT /api/records/{id}/draft; Left (status, message) so that a rejected payload (400) can be retried reduced
updateDraft :: WizardRequestContextC s m => ZenodoClient -> String -> Value -> m (Either (Maybe Int, String) Value)
updateDraft client recordId payload = do
  let operation = "Zenodo metadata update"
  eResponse <- zenodoRequest operation "PUT" (recordsUrl client [recordId, "draft"]) (jsonHeaders client) (BSL.toStrict (encode payload))
  return $
    case eResponse of
      Left error -> Left (Nothing, error)
      Right response ->
        case expectJson operation [200] response of
          Right value -> Right value
          Left error -> Left (Just response.status, error)

-- POST /api/records/{id}/versions
createNewVersion :: WizardRequestContextC s m => ZenodoClient -> String -> m (Either String Value)
createNewVersion client recordId = jsonCall client "Zenodo new version creation" "POST" (recordsUrl client [recordId, "versions"]) Nothing [201]

-- DELETE /api/records/{id}/draft
deleteDraft :: WizardRequestContextC s m => ZenodoClient -> String -> m (Either String ())
deleteDraft client recordId = emptyCall client "Zenodo draft cleanup" "DELETE" (recordsUrl client [recordId, "draft"]) Nothing [200, 202, 204]

-- GET /api/records/{id}/draft/files
listDraftFileKeys :: WizardRequestContextC s m => ZenodoClient -> String -> m (Either String [String])
listDraftFileKeys client recordId = do
  eValue <- jsonCall client "Zenodo file listing" "GET" (recordsUrl client [recordId, "draft", "files"]) Nothing [200]
  return $ fmap fileKeys eValue
  where
    fileKeys (Object o) =
      case KM.lookup "entries" o of
        Just (Array entries) -> [T.unpack key | Object entry <- V.toList entries, Just (String key) <- [KM.lookup "key" entry]]
        _ -> []
    fileKeys _ = []

-- DELETE /api/records/{id}/draft/files/{key}
deleteDraftFile :: WizardRequestContextC s m => ZenodoClient -> String -> String -> m (Either String ())
deleteDraftFile client recordId key = emptyCall client "Zenodo file deletion" "DELETE" (draftFileUrl client recordId key) Nothing [204]

-- The three steps of an upload: initialise the file, send its content and commit it
uploadDraftFile :: WizardRequestContextC s m => ZenodoClient -> String -> String -> BS.ByteString -> m (Either String ())
uploadDraftFile client recordId key content = do
  eInit <- emptyCall client "Zenodo file upload initialisation" "POST" (recordsUrl client [recordId, "draft", "files"]) (Just (toJSON [object ["key" .= key]])) [201]
  case eInit of
    Left error -> return (Left error)
    Right () -> do
      let operation = "Zenodo file content upload"
      let headers = M.fromList [("Authorization", "Bearer " ++ client.accessToken), ("Content-Type", "application/octet-stream")]
      eResponse <- zenodoRequest operation "PUT" (draftFileUrl client recordId key ++ "/content") headers content
      case eResponse >>= expectStatus operation [200, 201] of
        Left error -> return (Left error)
        Right _ -> emptyCall client "Zenodo file upload commit" "POST" (draftFileUrl client recordId key ++ "/commit") Nothing [200, 201]

-- GET /api/communities/{slug or id}; the id of the community
resolveCommunityId :: WizardRequestContextC s m => ZenodoClient -> String -> m (Either String String)
resolveCommunityId client community = do
  let identifier = filter (/= '/') community
  eValue <- jsonCall client "Zenodo community lookup" "GET" (client.baseUrl ++ "/api/communities/" ++ encodeSegment identifier) Nothing [200]
  return $
    eValue >>= \case
      Object o | Just (String communityId) <- KM.lookup "id" o -> Right (T.unpack communityId)
      _ -> Left "Unexpected response from Zenodo community lookup."

-- PUT /api/records/{id}/draft/review: creates the community submission request without submitting it, so the draft
-- stays unpublished and owned by the depositor
attachToCommunity :: WizardRequestContextC s m => ZenodoClient -> String -> String -> m (Either String ())
attachToCommunity client recordId communityId =
  emptyCall
    client
    "Zenodo community attachment"
    "PUT"
    (recordsUrl client [recordId, "draft", "review"])
    (Just (object ["type" .= ("community-submission" :: String), "receiver" .= object ["community" .= communityId]]))
    [200, 201]

-- --------------------------------
-- PRIVATE
-- --------------------------------
jsonCall :: WizardRequestContextC s m => ZenodoClient -> String -> String -> String -> Maybe Value -> [Int] -> m (Either String Value)
jsonCall client operation method url mBody expected = do
  eResponse <- zenodoRequest operation method url (jsonHeaders client) (maybe BS.empty (BSL.toStrict . encode) mBody)
  return (eResponse >>= expectJson operation expected)

emptyCall :: WizardRequestContextC s m => ZenodoClient -> String -> String -> String -> Maybe Value -> [Int] -> m (Either String ())
emptyCall client operation method url mBody expected = do
  eResponse <- zenodoRequest operation method url (jsonHeaders client) (maybe BS.empty (BSL.toStrict . encode) mBody)
  return (() <$ (eResponse >>= expectStatus operation expected))

optionalCall :: WizardRequestContextC s m => ZenodoClient -> String -> String -> m (Either String (Maybe Value))
optionalCall client operation url = do
  eResponse <- zenodoRequest operation "GET" url (jsonHeaders client) BS.empty
  return $
    eResponse >>= \response ->
      if response.status `elem` [404, 410]
        then Right Nothing
        else Just <$> expectJson operation [200] response

jsonHeaders :: ZenodoClient -> M.Map String String
jsonHeaders client = M.fromList [("Authorization", "Bearer " ++ client.accessToken), ("Content-Type", "application/json"), ("Accept", "application/json")]

recordsUrl :: ZenodoClient -> [String] -> String
recordsUrl client segments = client.baseUrl ++ "/api/records" ++ concatMap ('/' :) segments

draftFileUrl :: ZenodoClient -> String -> String -> String
draftFileUrl client recordId key = recordsUrl client [recordId, "draft", "files", encodeSegment key]

encodeSegment :: String -> String
encodeSegment = BS.unpack . urlEncode False . TE.encodeUtf8 . T.pack
