module Shared.Database.Mapping.Project.ProjectEventList where

import Data.Maybe (fromJust, listToMaybe)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Database.Mapping.Project.ProjectEvent
import Shared.Database.Mapping.User.UserSuggestion
import Shared.Model.Project.Event.ProjectEventList
import Shared.Util.Uuid

instance FromRow ProjectEventList where
  fromRow = do
    uuid <- field
    aType <- field
    mPath <- field
    createdAt <- field
    valueType <- field
    valueText <- fieldValueText
    mValueRaw <- field
    createdBy <- fieldUserSuggestion'
    case aType of
      SetReplyEventType -> do
        return . SetReplyEventList' $
          SetReplyEventList
            { uuid = uuid
            , path = fromJust mPath
            , value = parseValue valueType valueText mValueRaw
            , createdBy = createdBy
            , createdAt = createdAt
            }
      ClearReplyEventType ->
        let path = fromJust mPath
         in return . ClearReplyEventList' $ ClearReplyEventList {..}
      SetPhaseEventType ->
        return . SetPhaseEventList' $
          SetPhaseEventList
            { uuid = uuid
            , phaseUuid = parsePhaseUuid valueText
            , createdBy = createdBy
            , createdAt = createdAt
            }
      SetLabelsEventType ->
        return . SetLabelsEventList' $
          SetLabelsEventList
            { uuid = uuid
            , path = fromJust mPath
            , value = fmap u' valueText
            , createdBy = createdBy
            , createdAt = createdAt
            }
      DeleteItemEventType ->
        return . DeleteItemEventList' $
          DeleteItemEventList
            { uuid = uuid
            , path = fromJust mPath
            , itemUuid = u' . head $ valueText
            , itemTitle = listToMaybe . drop 1 $ valueText
            , createdBy = createdBy
            , createdAt = createdAt
            }
