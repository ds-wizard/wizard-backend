module Shared.Service.KnowledgeModel.KnowledgeModelService where

import Control.Monad.Except (liftEither)
import Control.Monad.Reader (asks, liftIO)
import Data.Time
import qualified Data.UUID as U

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelChangeDTO
import Shared.Database.DAO.KnowledgeModel.KnowledgeModelCacheDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.Database.Mapping.KnowledgeModel.Bundle.KnowledgeModelBundlePackage ()
import Shared.Model.Context.WizardRequestContext
import Shared.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelCache
import Shared.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.Service.KnowledgeModel.Compiler.Compiler
import Shared.Service.KnowledgeModel.KnowledgeModelFilter
import Shared.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Shared.Service.KnowledgeModel.Package.WizardKnowledgeModelPackageUtil

createKnowledgeModelPreview :: WizardRequestContextC s m => KnowledgeModelChangeDTO -> m KnowledgeModel
createKnowledgeModelPreview reqDto = do
  checkViewPermissionToKnowledgeModelPackage reqDto.knowledgeModelPackageUuid
  compileKnowledgeModel reqDto.events reqDto.knowledgeModelPackageUuid reqDto.tagUuids

compileKnowledgeModel :: WizardRequestContextC s m => [KnowledgeModelEvent] -> Maybe U.UUID -> [U.UUID] -> m KnowledgeModel
compileKnowledgeModel events mPkgUuid tagUuids = compileKnowledgeModelWithCaching' events mPkgUuid tagUuids True

compileKnowledgeModelWithCaching' :: WizardRequestContextC s m => [KnowledgeModelEvent] -> Maybe U.UUID -> [U.UUID] -> Bool -> m KnowledgeModel
compileKnowledgeModelWithCaching' [] (Just pkgUuid) tagUuids True = findOrCompileKnowledgeModel pkgUuid tagUuids
compileKnowledgeModelWithCaching' events (Just pkgUuid) tagUuids True = do
  km <- findOrCompileKnowledgeModel pkgUuid []
  filterKnowledgeModel tagUuids <$> liftEither (compile (Just km) events)
compileKnowledgeModelWithCaching' events mPkgUuid tagUuids _ = do
  allEvents <- getEvents mPkgUuid
  km <- liftEither $ compile Nothing allEvents
  return $ filterKnowledgeModel tagUuids km
  where
    getEvents Nothing = return events
    getEvents (Just pkgUuid) = do
      (pkgs :: [KnowledgeModelBundlePackage]) <- findSeriesOfPackagesRecursiveByUuid pkgUuid
      return $ concatMap (.events) pkgs ++ events

findOrCompileKnowledgeModel :: WizardRequestContextC s m => U.UUID -> [U.UUID] -> m KnowledgeModel
findOrCompileKnowledgeModel pkgUuid tagUuids = do
  tenantUuid <- asks (.tenantUuid')
  mKmCache <- findKnowledgeModelCacheByUuid' pkgUuid tagUuids tenantUuid
  case mKmCache of
    Just kmCache -> return kmCache.knowledgeModel
    Nothing -> do
      km <- compileMissingKnowledgeModel pkgUuid tagUuids
      createdAt <- liftIO getCurrentTime
      insertKnowledgeModelCache KnowledgeModelCache {knowledgeModelPackageUuid = pkgUuid, tagUuids = tagUuids, knowledgeModel = km, tenantUuid = tenantUuid, createdAt = createdAt}
      return km

compileMissingKnowledgeModel :: WizardRequestContextC s m => U.UUID -> [U.UUID] -> m KnowledgeModel
compileMissingKnowledgeModel pkgUuid [] = do
  pkg <- findPackageByUuid pkgUuid
  mPreviousKm <- traverse (`findOrCompileKnowledgeModel` []) pkg.previousPackageUuid
  events <- findPackageEvents pkgUuid
  liftEither $ compile mPreviousKm (fmap toEvent events)
compileMissingKnowledgeModel pkgUuid tagUuids = filterKnowledgeModel tagUuids <$> findOrCompileKnowledgeModel pkgUuid []
