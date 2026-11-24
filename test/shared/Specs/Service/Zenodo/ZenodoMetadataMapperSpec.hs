module Specs.Service.Zenodo.ZenodoMetadataMapperSpec where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (fromJust, isJust)
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Shared.Service.Zenodo.ZenodoMetadataMapper
import Shared.Service.Zenodo.ZenodoSubmitter (uniqueFileKeys)

-- ORCID 0000-0002-1825-0097 is the canonical valid test identifier
validOrcid :: String
validOrcid = "0000-0002-1825-0097"

invalidOrcid :: String
invalidOrcid = "0000-0002-1825-0098"

madmpObject :: [(Key, Value)] -> Value
madmpObject overrides =
  let base =
        KM.fromList
          [ ("title", "Seabird tracking DMP")
          , ("description", "DMP level description.")
          , ("modified", "2026-03-14T11:34:00+00:00")
          , ("language", "eng")
          , ("dmp_id", object ["identifier" .= ("https://doi.org/10.1234/dmp.1" :: String), "type" .= ("doi" :: String)])
          ,
            ( "contact"
            , object
                [ "name" .= ("Lovelace, Ada" :: String)
                , "contact_id" .= object ["identifier" .= ("https://orcid.org/" ++ validOrcid), "type" .= ("orcid" :: String)]
                , "affiliation" .= object ["name" .= ("University of Bergen" :: String), "affiliation_id" .= object ["identifier" .= ("https://ror.org/03zga2b32" :: String), "type" .= ("ror" :: String)]]
                ]
            )
          ,
            ( "contributor"
            , toJSON
                [ object
                    [ "name" .= ("Hopper, Grace" :: String)
                    , "role" .= ["Data Manager" :: String]
                    , "contributor_id" .= object ["identifier" .= validOrcid, "type" .= ("orcid" :: String)]
                    , "affiliation" .= object ["name" .= ("Naval Research Lab" :: String)]
                    ]
                ]
            )
          ,
            ( "project"
            , toJSON
                [ object
                    [ "title" .= ("Seabird Tracking" :: String)
                    , "description" .= ("Project level description." :: String)
                    , "funding"
                        .= [ object
                              [ "name" .= ("European Commission" :: String)
                              , "funder_id" .= object ["identifier" .= ("https://ror.org/00k4n6c32" :: String), "type" .= ("ror" :: String)]
                              , "grant_id" .= object ["identifier" .= ("776242" :: String), "type" .= ("other" :: String)]
                              ]
                           ]
                    ]
                ]
            )
          , ("dataset", toJSON [object ["dataset_id" .= object ["identifier" .= ("10.5281/zenodo.999" :: String), "type" .= ("doi" :: String)]]])
          ]
   in Object (KM.union (KM.fromList overrides) base)

madmp :: [(Key, Value)] -> BSL.ByteString
madmp overrides = encode (object ["dmp" .= madmpObject overrides])

parsed :: [(Key, Value)] -> MadmpMetadata
parsed = fromJust . parseMadmp . madmp

defaults :: RecordDefaults
defaults =
  RecordDefaults
    { title = "Wizard Project"
    , description = "Wizard description."
    , resourceType = "publication-datamanagementplan"
    , publicationDate = "2026-07-30"
    , creatorName = "Bosl, Korbinian"
    , creatorAffiliation = Just "University of Bergen"
    , projectUrl = Just "https://dsw.example/projects/1"
    , licenseId = "cc-by-4.0"
    , accessLevel = "public"
    }

at :: [String] -> Value -> Value
at [] value = value
at (key : keys) (Object o) = at keys (KM.findWithDefault Null (K.fromString key) o)
at _ _ = Null

hasKey :: [String] -> String -> Value -> Bool
hasKey path key value =
  case at path value of
    Object o -> KM.member (K.fromString key) o
    _ -> False

person :: String -> String -> [Pair] -> Value
person familyName givenName extra = object (["type" .= ("personal" :: String), "family_name" .= familyName, "given_name" .= givenName] ++ extra)

orcidIdentifiers :: [Pair]
orcidIdentifiers = ["identifiers" .= [object ["scheme" .= ("orcid" :: String), "identifier" .= validOrcid]]]

zenodoMetadataMapperSpec =
  describe "ZenodoMetadataMapper" $ do
    describe "parseMadmp" $ do
      it "extracts the core fields from a wrapped DMP" $ do
        let metadata = parsed []
        metadata.title `shouldBe` Just "Seabird tracking DMP"
        metadata.description `shouldBe` Just "Project level description."
        metadata.publicationDate `shouldBe` Just "2026-03-14"
        metadata.languageId `shouldBe` Just "eng"
      it "accepts an unwrapped DMP document" $
        (fmap (.title) . parseMadmp . encode $ madmpObject []) `shouldBe` Just (Just "Seabird tracking DMP")
      it "takes the creators from the contributors with split names and ORCID" $
        (parsed []).creators `shouldBe` [object ["person_or_org" .= person "Hopper" "Grace" orcidIdentifiers, "affiliations" .= [object ["name" .= ("Naval Research Lab" :: String)]]]]
      it "turns the contact into a contact person with ROR affiliation" $
        (parsed []).contributors
          `shouldBe` [ object
                        [ "person_or_org" .= person "Lovelace" "Ada" orcidIdentifiers
                        , "affiliations" .= [object ["id" .= ("03zga2b32" :: String)]]
                        , "role" .= object ["id" .= ("contactperson" :: String)]
                        ]
                     ]
      it "falls back to the contact when there are no contributors" $
        fmap (at ["person_or_org", "family_name"]) (parsed [("contributor", toJSON ([] :: [Value]))]).creators `shouldBe` [String "Lovelace"]
      it "drops an ORCID failing the checksum" $ do
        let metadata = parsed [("contributor", toJSON [object ["name" .= ("Hopper, Grace" :: String), "contributor_id" .= object ["identifier" .= invalidOrcid, "type" .= ("orcid" :: String)]]])]
        hasKey ["person_or_org"] "identifiers" (head metadata.creators) `shouldBe` False
      it "covers the DMP id and the dataset DOIs as related identifiers" $
        (parsed []).relatedIdentifiers
          `shouldBe` [ object ["identifier" .= ("10.1234/dmp.1" :: String), "scheme" .= ("doi" :: String), "relation_type" .= object ["id" .= ("isidenticalto" :: String)], "resource_type" .= object ["id" .= ("publication-datamanagementplan" :: String)]]
                     , object ["identifier" .= ("10.5281/zenodo.999" :: String), "scheme" .= ("doi" :: String), "relation_type" .= object ["id" .= ("describes" :: String)], "resource_type" .= object ["id" .= ("dataset" :: String)]]
                     ]
      it "uses the bare ROR and the award number for funding" $
        (parsed []).funding `shouldBe` [object ["funder" .= object ["id" .= ("00k4n6c32" :: String)], "award" .= object ["number" .= ("776242" :: String), "title" .= object ["en" .= ("Seabird Tracking" :: String)]]]]
      it "falls back to the funder name without ROR" $
        (parsed [("project", toJSON [object ["title" .= ("Seabird Tracking" :: String), "funding" .= [object ["name" .= ("Research Council of Norway" :: String)]]]])]).funding
          `shouldBe` [object ["funder" .= object ["name" .= ("Research Council of Norway" :: String)]]]
      it "returns nothing for malformed JSON" $
        isJust (parseMadmp (BSL.pack "not json at all")) `shouldBe` False
      it "returns nothing for an unrelated JSON document" $
        isJust (parseMadmp (BSL.pack "{\"unrelated\": true}")) `shouldBe` False
    describe "buildRecordPayload" $ do
      it "applies the license and access defaults" $ do
        let payload = buildRecordPayload defaults Nothing
        at ["access"] payload `shouldBe` object ["record" .= ("public" :: String), "files" .= ("public" :: String)]
        at ["files"] payload `shouldBe` object ["enabled" .= True]
        at ["metadata", "rights"] payload `shouldBe` toJSON [object ["id" .= ("cc-by-4.0" :: String)]]
      it "applies restricted access to the record and the files" $
        at ["access"] (buildRecordPayload defaults {accessLevel = "restricted"} Nothing) `shouldBe` object ["record" .= ("restricted" :: String), "files" .= ("restricted" :: String)]
      it "falls back to public for an unknown access level" $
        at ["access"] (buildRecordPayload defaults {accessLevel = "nonsense"} Nothing) `shouldBe` object ["record" .= ("public" :: String), "files" .= ("public" :: String)]
      it "uses the wizard defaults without a maDMP" $ do
        let payload = buildRecordPayload defaults Nothing
        at ["metadata", "title"] payload `shouldBe` String "Wizard Project"
        at ["metadata", "description"] payload `shouldBe` String "Wizard description."
        at ["metadata", "publication_date"] payload `shouldBe` String "2026-07-30"
        at ["metadata", "resource_type"] payload `shouldBe` object ["id" .= ("publication-datamanagementplan" :: String)]
        at ["metadata", "creators"] payload
          `shouldBe` toJSON [object ["person_or_org" .= object ["type" .= ("personal" :: String), "family_name" .= ("Bosl" :: String), "given_name" .= ("Korbinian" :: String)], "affiliations" .= [object ["name" .= ("University of Bergen" :: String)]]]]
        hasKey ["metadata"] "contributors" payload `shouldBe` False
        hasKey ["metadata"] "funding" payload `shouldBe` False
      it "prefers the maDMP over the wizard defaults" $ do
        let payload = buildRecordPayload defaults (Just (parsed []))
        at ["metadata", "title"] payload `shouldBe` String "Seabird tracking DMP"
        at ["metadata", "description"] payload `shouldBe` String "Project level description."
        at ["metadata", "publication_date"] payload `shouldBe` String "2026-03-14"
        at ["metadata", "languages"] payload `shouldBe` toJSON [object ["id" .= ("eng" :: String)]]
      it "fills the gaps of the maDMP with the wizard defaults" $ do
        let payload = buildRecordPayload defaults (Just (parsed [("title", Null), ("modified", Null), ("language", Null)]))
        at ["metadata", "title"] payload `shouldBe` String "Seabird Tracking"
        at ["metadata", "publication_date"] payload `shouldBe` String "2026-07-30"
        hasKey ["metadata"] "languages" payload `shouldBe` False
      it "keeps the project URL next to the related identifiers of the maDMP" $ do
        let payload = buildRecordPayload defaults (Just (parsed []))
        case at ["metadata", "related_identifiers"] payload of
          Array identifiers -> do
            length identifiers `shouldBe` 3
            at ["identifier"] (head (foldr (:) [] identifiers)) `shouldBe` String "https://dsw.example/projects/1"
          _ -> expectationFailure "related identifiers missing"
    describe "reduceRecordPayload" $ do
      it "drops the enriched blocks and keeps the required ones" $ do
        let reduced = reduceRecordPayload (buildRecordPayload defaults (Just (parsed [])))
        fmap (\key -> hasKey ["metadata"] key reduced) ["contributors", "funding", "related_identifiers", "languages"] `shouldBe` [False, False, False, False]
        at ["metadata", "title"] reduced `shouldBe` String "Seabird tracking DMP"
        at ["metadata", "rights"] reduced `shouldBe` toJSON [object ["id" .= ("cc-by-4.0" :: String)]]
      it "strips the identifiers and affiliations of the creators" $
        at ["metadata", "creators"] (reduceRecordPayload (buildRecordPayload defaults (Just (parsed []))))
          `shouldBe` toJSON [object ["person_or_org" .= object ["type" .= ("personal" :: String), "family_name" .= ("Hopper" :: String), "given_name" .= ("Grace" :: String)]]]
      it "is idempotent" $ do
        let once = reduceRecordPayload (buildRecordPayload defaults (Just (parsed [])))
        reduceRecordPayload once `shouldBe` once
      it "keeps an already minimal payload" $ do
        let payload = buildRecordPayload defaults {projectUrl = Nothing, creatorAffiliation = Nothing} Nothing
        reduceRecordPayload payload `shouldBe` payload
    describe "helpers" $ do
      it "splits names" $ do
        splitName "Lovelace, Ada" `shouldBe` (Just "Ada", "Lovelace")
        splitName "Ada King Lovelace" `shouldBe` (Just "Ada King", "Lovelace")
        splitName "Plato" `shouldBe` (Nothing, "Plato")
      it "validates the ORCID checksum" $ do
        isValidOrcid validOrcid `shouldBe` True
        isValidOrcid invalidOrcid `shouldBe` False
        isValidOrcid "0000-0002-1694-233X" `shouldBe` True
      it "makes the file keys unique" $
        uniqueFileKeys ["report.pdf", "report.pdf", "data.json", "report.pdf", "README"] `shouldBe` ["report.pdf", "report (2).pdf", "data.json", "report (3).pdf", "README"]
