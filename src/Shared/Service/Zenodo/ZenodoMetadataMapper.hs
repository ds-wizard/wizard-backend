-- Port of madmp_mapping.py of the Zenodo submission plugin: builds the InvenioRDM record payload of a submission from
-- the defaults of the wizard and the (optional) RDA DMP Common Standard JSON (maDMP) of one of its documents
module Shared.Service.Zenodo.ZenodoMetadataMapper (
  MadmpMetadata (..),
  RecordDefaults (..),
  parseMadmp,
  buildRecordPayload,
  reduceRecordPayload,
  splitName,
  isValidOrcid,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (isDigit, isSpace, toLower, toUpper)
import qualified Data.List as L
import Data.Maybe (catMaybes, isJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Regex.TDFA ((=~))

data MadmpMetadata = MadmpMetadata
  { title :: Maybe String
  , description :: Maybe String
  , publicationDate :: Maybe String
  , languageId :: Maybe String
  , creators :: [Value]
  , contributors :: [Value]
  , relatedIdentifiers :: [Value]
  , funding :: [Value]
  }
  deriving (Show, Eq)

data RecordDefaults = RecordDefaults
  { title :: String
  , description :: String
  , resourceType :: String
  , publicationDate :: String
  , creatorName :: String
  , creatorAffiliation :: Maybe String
  , projectUrl :: Maybe String
  , licenseId :: String
  , accessLevel :: String
  }
  deriving (Show, Eq)

-- --------------------------------
-- PARSING
-- --------------------------------
parseMadmp :: BSL.ByteString -> Maybe MadmpMetadata
parseMadmp raw = do
  value <- decode raw
  dmp <- unwrapDmp value
  let metadata =
        MadmpMetadata
          { title = dmpTitle dmp
          , description = dmpDescription dmp
          , publicationDate = dmpPublicationDate dmp
          , languageId = dmpLanguage dmp
          , creators = dmpCreators dmp
          , contributors = dmpContributors dmp
          , relatedIdentifiers = dmpRelatedIdentifiers dmp
          , funding = dmpFunding dmp
          }
  if isEmpty metadata then Nothing else Just metadata
  where
    isEmpty m =
      not (isJust m.title || isJust m.description || isJust m.publicationDate || isJust m.languageId)
        && null m.creators
        && null m.contributors
        && null m.relatedIdentifiers
        && null m.funding

unwrapDmp :: Value -> Maybe Object
unwrapDmp (Object o) =
  case KM.lookup "dmp" o of
    Just (Object inner) -> Just inner
    _ ->
      if any (`KM.member` o) ["title", "contact", "contributor", "dataset", "dmp_id", "project"]
        then Just o
        else Nothing
unwrapDmp _ = Nothing

dmpTitle :: Object -> Maybe String
dmpTitle dmp = firstJust (clean (KM.lookup "title" dmp) : [clean (KM.lookup "title" project) | project <- objects (KM.lookup "project" dmp)])

dmpDescription :: Object -> Maybe String
dmpDescription dmp = firstJust ([clean (KM.lookup "description" project) | project <- objects (KM.lookup "project" dmp)] ++ [clean (KM.lookup "description" dmp)])

dmpPublicationDate :: Object -> Maybe String
dmpPublicationDate dmp = firstJust [isoDate =<< clean (KM.lookup key dmp) | key <- ["modified", "created"]]
  where
    isoDate raw =
      let candidate = take 10 raw
       in if candidate =~ ("^[0-9]{4}-[0-9]{2}-[0-9]{2}$" :: String) then Just candidate else Nothing

dmpLanguage :: Object -> Maybe String
dmpLanguage dmp = do
  raw <- clean (KM.lookup "language" dmp)
  let candidate = fmap toLower raw
  if candidate =~ ("^[a-z]{3}$" :: String) then Just candidate else Nothing

dmpCreators :: Object -> [Value]
dmpCreators dmp =
  case mapMaybe (`agentEntry` "contributor_id") (objects (KM.lookup "contributor" dmp)) of
    [] -> maybe [] (\contact -> maybe [] (: []) (agentEntry contact "contact_id")) (asObject =<< KM.lookup "contact" dmp)
    creators -> creators

dmpContributors :: Object -> [Value]
dmpContributors dmp =
  case asObject =<< KM.lookup "contact" dmp of
    Just contact ->
      case agentEntry contact "contact_id" of
        Just (Object agent) -> [Object (KM.insert "role" (object ["id" .= ("contactperson" :: String)]) agent)]
        _ -> []
    Nothing -> []

agentEntry :: Object -> Key -> Maybe Value
agentEntry entry idKey = do
  person <- personOrOrg entry idKey
  let affiliations = entryAffiliations entry
  return . object $ ("person_or_org" .= person) : ["affiliations" .= affiliations | not (null affiliations)]

personOrOrg :: Object -> Key -> Maybe Value
personOrOrg entry idKey = do
  name <- clean (KM.lookup "name" entry)
  let (mGivenName, familyName) = splitName name
  let mOrcid = entryOrcid entry idKey
  return . object $
    [ "type" .= ("personal" :: String)
    , "family_name" .= familyName
    ]
      ++ ["given_name" .= givenName | Just givenName <- [mGivenName]]
      ++ ["identifiers" .= [object ["scheme" .= ("orcid" :: String), "identifier" .= orcid]] | Just orcid <- [mOrcid]]

-- "Last, First" or "First ... Last"
splitName :: String -> (Maybe String, String)
splitName name =
  case break (== ',') name of
    (familyPart, ',' : givenPart)
      | not (null (strip familyPart)) -> (nonEmpty (strip givenPart), strip familyPart)
    _ ->
      case words name of
        parts@(_ : _ : _) -> (Just (unwords (init parts)), last parts)
        _ -> (Nothing, name)

entryOrcid :: Object -> Key -> Maybe String
entryOrcid entry idKey = do
  block <- asObject =<< KM.lookup idKey entry
  let identifierType = maybe "" (fmap toLower) (clean (KM.lookup "type" block))
  if identifierType `notElem` ["orcid", ""]
    then Nothing
    else do
      raw <- clean (KM.lookup "identifier" block)
      let match = raw =~ ("[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9Xx]" :: String) :: String
      if null match
        then Nothing
        else
          let candidate = fmap toUpper match
           in if isValidOrcid candidate then Just candidate else Nothing

-- ISO 7064 MOD 11-2 checksum of an ORCID
isValidOrcid :: String -> Bool
isValidOrcid orcid =
  let digits = filter (/= '-') orcid
   in length digits == 16
        && all isDigit (take 15 digits)
        && let total = foldl (\acc c -> (acc + read [c]) * 2) (0 :: Int) (take 15 digits)
               remainder = (12 - total `mod` 11) `mod` 11
               expected = if remainder == 10 then 'X' else head (show remainder)
            in expected == last digits

entryAffiliations :: Object -> [Value]
entryAffiliations entry =
  case asObject =<< KM.lookup "affiliation" entry of
    Just affiliation ->
      case bareRor (KM.lookup "affiliation_id" affiliation) of
        Just ror -> [object ["id" .= ror]]
        Nothing -> maybe [] (\name -> [object ["name" .= name]]) (clean (KM.lookup "name" affiliation))
    Nothing -> []

bareRor :: Maybe Value -> Maybe String
bareRor mBlock = do
  block <- asObject =<< mBlock
  raw <- clean (KM.lookup "identifier" block)
  let candidate = fmap toLower . lastSegment $ raw
  if candidate =~ ("^0[0-9a-hj-km-np-tv-z]{6}[0-9]{2}$" :: String) then Just candidate else Nothing

dmpRelatedIdentifiers :: Object -> [Value]
dmpRelatedIdentifiers dmp =
  let dmpIdentifier = [relatedIdentifier identifier scheme "isidenticalto" "publication-datamanagementplan" | Just (identifier, scheme) <- [identifierEntry (KM.lookup "dmp_id" dmp)]]
      datasets = [relatedIdentifier identifier scheme "describes" "dataset" | dataset <- objects (KM.lookup "dataset" dmp), Just (identifier, scheme) <- [identifierEntry (KM.lookup "dataset_id" dataset)]]
   in L.nub (dmpIdentifier ++ datasets)

relatedIdentifier :: String -> String -> String -> String -> Value
relatedIdentifier identifier scheme relationType resourceType =
  object
    [ "identifier" .= identifier
    , "scheme" .= scheme
    , "relation_type" .= object ["id" .= relationType]
    , "resource_type" .= object ["id" .= resourceType]
    ]

identifierEntry :: Maybe Value -> Maybe (String, String)
identifierEntry mBlock = do
  block <- asObject =<< mBlock
  raw <- clean (KM.lookup "identifier" block)
  case normalizeDoi raw of
    Just doi -> Just (doi, "doi")
    Nothing
      | "http://" `L.isPrefixOf` raw || "https://" `L.isPrefixOf` raw -> Just (raw, "url")
      | otherwise -> Nothing

normalizeDoi :: String -> Maybe String
normalizeDoi value =
  let lowered = fmap toLower value
      prefixes = ["https://doi.org/", "http://doi.org/", "https://dx.doi.org/", "http://dx.doi.org/", "doi:"]
      candidate = maybe value (\prefix -> drop (length prefix) value) (L.find (`L.isPrefixOf` lowered) prefixes)
   in if candidate =~ ("^10\\.[0-9]{4,9}/[^[:space:]]+$" :: String) then Just candidate else Nothing

dmpFunding :: Object -> [Value]
dmpFunding dmp =
  L.nub
    [ entry
    | project <- objects (KM.lookup "project" dmp)
    , funding <- objects (KM.lookup "funding" project)
    , Just entry <- [fundingEntry funding (clean (KM.lookup "title" project))]
    ]

fundingEntry :: Object -> Maybe String -> Maybe Value
fundingEntry funding mProjectTitle = do
  funder <-
    case bareRor (KM.lookup "funder_id" funding) of
      Just ror -> Just (object ["id" .= ror])
      Nothing -> (\name -> object ["name" .= name]) <$> firstJust [clean (KM.lookup "name" funding), clean (KM.lookup "funder_name" funding)]
  let mAward = do
        number <- grantNumber (KM.lookup "grant_id" funding)
        return $ object ["number" .= number, "title" .= object ["en" .= maybe number id mProjectTitle]]
  return . object $ ("funder" .= funder) : ["award" .= award | Just award <- [mAward]]

grantNumber :: Maybe Value -> Maybe String
grantNumber mBlock = do
  block <- asObject =<< mBlock
  raw <- clean (KM.lookup "identifier" block)
  if "http://" `L.isPrefixOf` raw || "https://" `L.isPrefixOf` raw
    then nonEmpty (strip (lastSegment raw))
    else Just raw

-- --------------------------------
-- PAYLOAD
-- --------------------------------
buildRecordPayload :: RecordDefaults -> Maybe MadmpMetadata -> Value
buildRecordPayload defaults mMadmp =
  object
    [ "access" .= object ["record" .= accessLevel, "files" .= accessLevel]
    , "files" .= object ["enabled" .= True]
    , "metadata" .= object metadata
    ]
  where
    accessLevel = if defaults.accessLevel `elem` ["public", "restricted"] then defaults.accessLevel else "public"
    prefer get fallback = maybe fallback id (mMadmp >>= get)
    madmpList get = maybe [] get mMadmp
    metadata =
      [ "resource_type" .= object ["id" .= defaults.resourceType]
      , "title" .= prefer (.title) defaults.title
      , "description" .= prefer (.description) defaults.description
      , "publication_date" .= prefer (.publicationDate) defaults.publicationDate
      , "creators" .= creatorsWithFallback
      , "rights" .= [object ["id" .= defaults.licenseId]]
      ]
        ++ ["contributors" .= madmpList (.contributors) | not (null (madmpList (.contributors)))]
        ++ ["related_identifiers" .= relatedIdentifiers | not (null relatedIdentifiers)]
        ++ ["funding" .= madmpList (.funding) | not (null (madmpList (.funding)))]
        ++ ["languages" .= [object ["id" .= language]] | Just language <- [mMadmp >>= (.languageId)]]
    relatedIdentifiers =
      let projectIdentifier = [relatedIdentifier url "url" "compiles" "other" | Just url <- [defaults.projectUrl]]
       in projectIdentifier ++ filter (`notElem` projectIdentifier) (madmpList (.relatedIdentifiers))
    creatorsWithFallback =
      case madmpList (.creators) of
        [] ->
          let (mGivenName, familyName) = splitName defaults.creatorName
              person = object $ ["type" .= ("personal" :: String), "family_name" .= familyName] ++ ["given_name" .= givenName | Just givenName <- [mGivenName]]
           in [object $ ("person_or_org" .= person) : ["affiliations" .= [object ["name" .= affiliation]] | Just affiliation <- [defaults.creatorAffiliation]]]
        creators -> creators

-- The minimal record Zenodo accepts (used when it rejects the enriched one): without contributors, funding, related
-- identifiers and languages, and with creators reduced to their names
reduceRecordPayload :: Value -> Value
reduceRecordPayload (Object payload) =
  case KM.lookup "metadata" payload of
    Just (Object metadata) ->
      let withoutEnriched = foldr KM.delete metadata ["contributors", "funding", "related_identifiers", "languages"]
          reducedMetadata =
            case KM.lookup "creators" withoutEnriched of
              Just creators -> KM.insert "creators" (reduceCreators creators) withoutEnriched
              Nothing -> withoutEnriched
       in Object (KM.insert "metadata" (Object reducedMetadata) payload)
    _ -> Object payload
  where
    reduceCreators (Array creators) = Array (fmap plainCreator creators)
    reduceCreators value = value
    plainCreator (Object creator) =
      case KM.lookup "person_or_org" creator of
        Just (Object person) -> object ["person_or_org" .= Object (KM.fromList [(key, value) | (key, value) <- KM.toList person, key `elem` plainKeys])]
        _ -> Object creator
    plainCreator value = value
    plainKeys :: [Key]
    plainKeys = ["type", "name", "family_name", "given_name"]
reduceRecordPayload value = value

-- --------------------------------
-- HELPERS
-- --------------------------------
asObject :: Value -> Maybe Object
asObject (Object o) = Just o
asObject _ = Nothing

objects :: Maybe Value -> [Object]
objects (Just (Array values)) = mapMaybe asObject (V.toList values)
objects _ = []

clean :: Maybe Value -> Maybe String
clean (Just (String text)) = nonEmpty (strip (T.unpack text))
clean _ = Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust values =
  case catMaybes values of
    (value : _) -> Just value
    [] -> Nothing

strip :: String -> String
strip = L.dropWhileEnd isSpace . dropWhile isSpace

nonEmpty :: String -> Maybe String
nonEmpty "" = Nothing
nonEmpty value = Just value

lastSegment :: String -> String
lastSegment = reverse . takeWhile (/= '/') . dropWhile (== '/') . reverse
