module Shared.Api.Handler.Submission.Api where

import Servant
import Servant.Swagger.Tags

import Shared.Api.Handler.Submission.Detail_GET
import Shared.Api.Handler.Submission.List_Available_GET
import Shared.Api.Handler.Submission.List_GET
import Shared.Api.Handler.Submission.List_POST
import Shared.Api.Handler.WizardCommon

type SubmissionAPI =
  Tags "Project Submission"
    :> ( List_Available_GET
           :<|> List_GET
           :<|> List_POST
           :<|> Detail_GET
       )

submissionApi :: Proxy SubmissionAPI
submissionApi = Proxy

submissionServer :: WizardHandlerC s sm r rm => ServerT SubmissionAPI sm
submissionServer = list_available_GET :<|> list_GET :<|> list_POST :<|> detail_GET
