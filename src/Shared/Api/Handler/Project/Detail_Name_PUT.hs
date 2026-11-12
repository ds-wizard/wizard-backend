module Shared.Api.Handler.Project.Detail_Name_PUT where

import qualified Data.UUID as U
import Servant

import Shared.Api.Handler.Common
import Shared.Api.Handler.WizardCommon
import Shared.Api.Resource.Project.ProjectNameChangeDTO
import Shared.Api.Resource.Project.ProjectNameChangeJM ()
import Shared.Model.Context.TransactionState
import Shared.Service.Project.ProjectService

type Detail_Name_PUT =
  Header "Authorization" String
    :> Header "Host" String
    :> ReqBody '[SafeJSON] ProjectNameChangeDTO
    :> "projects"
    :> Capture "uuid" U.UUID
    :> "name"
    :> Put '[SafeJSON] (Headers '[Header "x-trace-uuid" String] ProjectNameChangeDTO)

detail_name_PUT
  :: WizardHandlerC s sm r rm
  => Maybe String
  -> Maybe String
  -> ProjectNameChangeDTO
  -> U.UUID
  -> sm (Headers '[Header "x-trace-uuid" String] ProjectNameChangeDTO)
detail_name_PUT mTokenHeader mServerUrl reqDto uuid =
  getAuthServiceExecutor mTokenHeader mServerUrl $ \runInAuthService ->
    runInAuthService Transactional $ addTraceUuidHeader =<< modifyProjectName uuid reqDto
