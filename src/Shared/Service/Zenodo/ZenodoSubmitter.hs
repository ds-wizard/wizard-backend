-- Sends the documents of a submission to Zenodo as an (unpublished) draft record (port of the submit-draft flow of the
-- Zenodo submission plugin): the draft of a previous submission is reused while unpublished, a published record gets a
-- new version, and a draft created by a failed submission is deleted again
module Shared.Service.Zenodo.ZenodoSubmitter (
  ZenodoTarget (..),
  submitToZenodo,
  uniqueFileKeys,
) where

import Control.Monad.Reader (liftIO)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as U

import Shared.Database.DAO.Submission.SubmissionDAO
import Shared.Database.DAO.User.UserDAO
import Shared.Integration.Http.Zenodo.ZenodoRecords
import Shared.Model.Context.WizardRequestContext
import Shared.Model.Document.Document
import Shared.Model.Project.Project
import Shared.Model.Settings.Settings
import Shared.Model.Submission.Submission
import Shared.Model.Submission.SubmissionResult
import Shared.Model.User.User
import Shared.Model.Zenodo.ZenodoRepository
import Shared.S3.Document.DocumentS3
import Shared.Service.Tenant.TenantHelper (getClientUrl)
import Shared.Model.OAuth.OAuthProvider
import Shared.Service.OAuth.OAuthService
import Shared.Service.Zenodo.ZenodoMetadataMapper

data ZenodoTarget = ZenodoTarget
  { repository :: ZenodoRepository
  , resourceType :: String
  , licenseId :: String
  , accessLevel :: String
  , community :: Maybe String
  }

-- The documents come with a flag whether they are a maDMP to read the metadata from
submitToZenodo :: WizardRequestContextC s m => Submission -> ZenodoTarget -> Project -> [(Document, Bool)] -> m (Either String SubmissionResult)
submitToZenodo submission target project documents =
  case submission.createdBy of
    Nothing -> return . Left $ "The user who created the submission does not exist anymore."
    Just userUuid -> do
      eToken <- getValidAccessToken userUuid Nothing (ZenodoOAuthProvider target.repository)
      case eToken of
        Left error -> return (Left error)
        Right token -> do
          let client = ZenodoClient {baseUrl = zenodoBaseUrl target.repository, accessToken = token}
          files <- traverse (\(document, _) -> retrieveDocumentContent document.uuid) documents
          let keys = uniqueFileKeys (fmap (documentFileName . fst) documents)
          let madmp = listToMaybe (catMaybes [parseMadmp (BSL.fromStrict content) | ((_, isMadmp), content) <- zip documents files, isMadmp])
          defaults <- recordDefaults target project userUuid
          let payload = buildRecordPayload defaults madmp
          let reducedPayload = reduceRecordPayload payload
          eDraft <- resolveDraft client submission reducedPayload userUuid
          case eDraft of
            Left error -> return (Left error)
            Right (recordId, createdNew) -> do
              result <- fillDraft client target recordId (zip keys files) payload reducedPayload
              case result of
                Left error -> do
                  -- Do not leave an empty draft behind; drafts of previous submissions are kept
                  if createdNew then () <$ deleteDraft client recordId else return ()
                  return (Left error)
                Right (draft, warnings) ->
                  return . Right $
                    SubmissionResult
                      { location = Just (fromMaybe (client.baseUrl ++ "/uploads/" ++ recordId) (selfHtmlLink draft))
                      , returnedData = if null warnings then Nothing else Just (L.intercalate "\n" warnings)
                      , externalId = Just recordId
                      , targetBaseUrl = Just client.baseUrl
                      }

-- The draft to fill: Right (record id, whether it was created now)
resolveDraft :: WizardRequestContextC s m => ZenodoClient -> Submission -> Value -> U.UUID -> m (Either String (String, Bool))
resolveDraft client submission reducedPayload userUuid = do
  mPrevious <- findLatestDoneSubmissionWithExternalId submission.projectUuid submission.definitionId client.baseUrl userUuid
  case mPrevious >>= (.externalId) of
    Nothing -> createNew
    Just previousId -> do
      eDraft <- getDraft client previousId
      case eDraft of
        Left error -> return (Left error)
        Right (Just _) -> withClearedFiles previousId False
        Right Nothing -> do
          eRecord <- getRecord client previousId
          case eRecord of
            Left error -> return (Left error)
            Right (Just _) -> do
              eVersion <- createNewVersion client previousId
              case eVersion >>= recordIdOf of
                Left error -> return (Left error)
                Right versionId -> withClearedFiles versionId True
            Right Nothing -> createNew
  where
    createNew = do
      eRecord <- createRecord client reducedPayload
      return $ fmap (\recordId -> (recordId, True)) (eRecord >>= recordIdOf)
    withClearedFiles recordId createdNew = do
      eKeys <- listDraftFileKeys client recordId
      case eKeys of
        Left error -> return (Left error)
        Right keys -> do
          results <- traverse (deleteDraftFile client recordId) keys
          return $ (recordId, createdNew) <$ sequence results

-- Uploads the files, sets the metadata (retried reduced when Zenodo rejects it) and attaches the draft to the community
fillDraft :: WizardRequestContextC s m => ZenodoClient -> ZenodoTarget -> String -> [(String, BS.ByteString)] -> Value -> Value -> m (Either String (Value, [String]))
fillDraft client target recordId files payload reducedPayload = do
  uploads <- traverse (uncurry (uploadDraftFile client recordId)) files
  case sequence uploads of
    Left error -> return (Left error)
    Right _ -> do
      eDraft <- updateDraft client recordId payload
      eMetadata <-
        case eDraft of
          Right draft -> return (Right (draft, []))
          Left (Just 400, error) | reducedPayload /= payload -> do
            eReduced <- updateDraft client recordId reducedPayload
            return $ case eReduced of
              Right draft -> Right (draft, ["Zenodo rejected part of the metadata, a reduced record was submitted (" ++ error ++ ")"])
              Left (_, reducedError) -> Left reducedError
          Left (_, error) -> return (Left error)
      case eMetadata of
        Left error -> return (Left error)
        Right (draft, warnings) -> do
          communityWarnings <- attachCommunity client target recordId
          return (Right (draft, warnings ++ communityWarnings))

attachCommunity :: WizardRequestContextC s m => ZenodoClient -> ZenodoTarget -> String -> m [String]
attachCommunity client target recordId =
  case filter (/= ' ') <$> target.community of
    Just community | not (null community) -> do
      eCommunityId <- resolveCommunityId client community
      eAttached <- either (return . Left) (attachToCommunity client recordId) eCommunityId
      return $ either (\error -> ["The draft was not added to the community " ++ community ++ ": " ++ error]) (const []) eAttached
    _ -> return []

recordDefaults :: WizardRequestContextC s m => ZenodoTarget -> Project -> U.UUID -> m RecordDefaults
recordDefaults target project userUuid = do
  mUser <- findUserByUuid' userUuid
  clientUrl <- getClientUrl
  today <- liftIO $ utctDay <$> getCurrentTime
  return
    RecordDefaults
      { title = project.name
      , description = fromMaybe "No description provided." (project.description >>= nonBlank)
      , resourceType = target.resourceType
      , publicationDate = showGregorian today
      , creatorName = maybe project.name (\user -> user.lastName ++ ", " ++ user.firstName) mUser
      , creatorAffiliation = mUser >>= (.affiliation) >>= nonBlank
      , projectUrl = Just (clientUrl ++ "/projects/" ++ U.toString project.uuid)
      , licenseId = target.licenseId
      , accessLevel = target.accessLevel
      }
  where
    nonBlank value = if all (== ' ') value then Nothing else Just value

documentFileName :: Document -> String
documentFileName document = fromMaybe document.name document.fileName

-- Zenodo needs a unique key per file: "report.pdf", "report (2).pdf", ...
uniqueFileKeys :: [String] -> [String]
uniqueFileKeys = reverse . foldl add []
  where
    add used name = uniqueName used name 1 : used
    uniqueName used name n =
      let (base, extension) = splitExtension name
          candidate = if n == (1 :: Int) then name else base ++ " (" ++ show n ++ ")" ++ extension
       in if candidate `elem` used then uniqueName used name (n + 1) else candidate

recordIdOf :: Value -> Either String String
recordIdOf (Object o) =
  case KM.lookup "id" o of
    Just (String recordId) -> Right (T.unpack recordId)
    Just (Number number) -> either (\_ -> Left "Unexpected record id from Zenodo.") (Right . show) (floatingOrInteger number :: Either Double Integer)
    _ -> Left "Zenodo did not return the id of the record."
recordIdOf _ = Left "Zenodo did not return the id of the record."

selfHtmlLink :: Value -> Maybe String
selfHtmlLink (Object o) =
  case KM.lookup "links" o of
    Just (Object links) ->
      case KM.lookup "self_html" links of
        Just (String link) -> Just (T.unpack link)
        _ -> Nothing
    _ -> Nothing
selfHtmlLink _ = Nothing

-- "report.pdf" -> ("report", ".pdf"); names without extension (or hidden files) keep everything in the base
splitExtension :: String -> (String, String)
splitExtension name =
  case L.elemIndices '.' name of
    [] -> (name, "")
    indices ->
      let index = last indices
       in if index == 0 then (name, "") else splitAt index name
