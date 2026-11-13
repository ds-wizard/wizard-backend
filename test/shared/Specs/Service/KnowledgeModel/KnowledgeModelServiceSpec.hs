module Specs.Service.KnowledgeModel.KnowledgeModelServiceSpec where

import Data.Maybe (isJust)
import Test.Hspec

import Shared.Constant.Tenant
import Shared.Util.Date

import Shared.Database.DAO.KnowledgeModel.KnowledgeModelCacheDAO
import Shared.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import qualified Shared.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KM_PKG_Migration
import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelCache
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.KnowledgeModel.KnowledgeModelService

import Specs.Common

knowledgeModelServiceSpec requestContext =
  describe "Knowledge Model Service" $ do
    it "compileKnowledgeModel compiles the same knowledge model as the whole series of packages" $
      -- GIVEN:
      do
        runInContextIO KM_PKG_Migration.runMigration requestContext
        (Right expectedKm) <- runInContext (compileKnowledgeModelWithCaching' [] (Just netherlandsKmPackageV2.uuid) [tagDataScience.uuid] False) requestContext
        -- WHEN:
        (Right km) <- runInContext (compileKnowledgeModel [] (Just netherlandsKmPackageV2.uuid) [tagDataScience.uuid]) requestContext
        -- THEN:
        km `shouldBe` expectedKm

    it "compileKnowledgeModel caches the knowledge model of every package in the series" $
      -- GIVEN:
      do
        runInContextIO KM_PKG_Migration.runMigration requestContext
        -- WHEN:
        (Right _) <- runInContext (compileKnowledgeModel [] (Just netherlandsKmPackageV2.uuid) [tagDataScience.uuid]) requestContext
        -- THEN:
        (Right mGlobalKmCache) <- runInContext (findKnowledgeModelCacheByUuid' globalKmPackage.uuid [] defaultTenantUuid) requestContext
        (Right mNetherlandsKmCache) <- runInContext (findKnowledgeModelCacheByUuid' netherlandsKmPackage.uuid [] defaultTenantUuid) requestContext
        (Right mNetherlandsV2KmCache) <- runInContext (findKnowledgeModelCacheByUuid' netherlandsKmPackageV2.uuid [] defaultTenantUuid) requestContext
        isJust mGlobalKmCache `shouldBe` True
        isJust mNetherlandsKmCache `shouldBe` True
        isJust mNetherlandsV2KmCache `shouldBe` True

    it "compileKnowledgeModel builds on the cached knowledge model of the previous package" $
      -- GIVEN:
      do
        runInContextIO KM_PKG_Migration.runMigration requestContext
        (Right netherlandsKm) <- runInContext (compileKnowledgeModelWithCaching' [] (Just netherlandsKmPackage.uuid) [] False) requestContext
        let cachedKm = netherlandsKm {annotations = [MapEntry "cached" "true"]} :: KnowledgeModel
        runInContextIO (insertKnowledgeModelCache (KnowledgeModelCache netherlandsKmPackage.uuid [] cachedKm defaultTenantUuid (dt' 2018 1 21))) requestContext
        -- WHEN:
        (Right km) <- runInContext (compileKnowledgeModel [] (Just netherlandsKmPackageV2.uuid) []) requestContext
        -- THEN:
        km.annotations `shouldBe` [MapEntry "cached" "true"]
