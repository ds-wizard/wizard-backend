module Shared.Service.Project.Compiler.ProjectCompilerService where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Shared.Model.Project.Event.ProjectEventList
import Shared.Model.Project.ProjectContent
import Shared.Model.Project.ProjectContentDM
import Shared.Model.Project.ProjectReply
import Shared.Service.Project.Event.ProjectEventMapper

compileProjectEvents :: [ProjectEventList] -> ProjectContent
compileProjectEvents = foldl applyEvent defaultProjectContent

applyEvent :: ProjectContent -> ProjectEventList -> ProjectContent
applyEvent projectContent (SetReplyEventList' event) = projectContent {replies = M.insert event.path (toReply' event) projectContent.replies}
applyEvent projectContent (ClearReplyEventList' event) = projectContent {replies = M.delete event.path projectContent.replies}
applyEvent projectContent (SetPhaseEventList' event) = projectContent {phaseUuid = event.phaseUuid}
applyEvent projectContent (SetLabelsEventList' event) =
  projectContent
    { labels = case event.value of
        [] -> M.delete event.path projectContent.labels
        newValue -> M.insert event.path newValue projectContent.labels
    }
applyEvent projectContent (DeleteItemEventList' event) =
  projectContent
    { replies = M.adjust removeFromReply event.path (deleteItemReplies event.path event.itemUuid projectContent.replies)
    , labels = deleteItemReplies event.path event.itemUuid projectContent.labels
    }
  where
    removeFromReply reply = Reply {value = removeItem event.itemUuid reply.value, createdBy = event.createdBy, createdAt = event.createdAt}

deleteItemReplies :: String -> U.UUID -> M.Map String a -> M.Map String a
deleteItemReplies path itemUuid = M.filterWithKey (\key _ -> not (itemPrefix `L.isPrefixOf` key))
  where
    itemPrefix = path ++ "." ++ U.toString itemUuid ++ "."

removeItem :: U.UUID -> ReplyValue -> ReplyValue
removeItem itemUuid (ItemListReply items) = ItemListReply (filter (/= itemUuid) items)
removeItem _ value = value
