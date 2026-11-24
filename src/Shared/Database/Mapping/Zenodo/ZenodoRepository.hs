module Shared.Database.Mapping.Zenodo.ZenodoRepository where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

import Shared.Database.Mapping.Common
import Shared.Model.Settings.Settings

instance ToField ZenodoRepository where
  toField = toFieldGenericEnum

instance FromField ZenodoRepository where
  fromField = fromFieldGenericEnum
