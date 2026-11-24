module WizardServer.Api.Handler.Settings.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Settings.List_GET
import Shared.Api.Handler.Settings.List_PUT
import Shared.Api.Handler.WizardCommon
import Shared.Model.Settings.Settings
import Shared.Service.Settings.OrganizationSettingsService

type SettingsRegistryAPI =
  Tags "Settings"
    :> ( List_GET "registry" SettingsRegistry
           :<|> List_PUT "registry" SettingsRegistry
       )

settingsRegistryApi :: Proxy SettingsRegistryAPI
settingsRegistryApi = Proxy

settingsRegistryServer :: WizardHandlerC s sm r rm => ServerT SettingsRegistryAPI sm
settingsRegistryServer = list_GET getSettingsRegistry :<|> list_PUT modifySettingsRegistry

type SettingsZenodoAPI =
  Tags "Settings"
    :> ( List_GET "zenodo" SettingsZenodo
           :<|> List_PUT "zenodo" SettingsZenodo
       )

settingsZenodoApi :: Proxy SettingsZenodoAPI
settingsZenodoApi = Proxy

settingsZenodoServer :: WizardHandlerC s sm r rm => ServerT SettingsZenodoAPI sm
settingsZenodoServer = list_GET getSettingsZenodo :<|> list_PUT modifySettingsZenodo
