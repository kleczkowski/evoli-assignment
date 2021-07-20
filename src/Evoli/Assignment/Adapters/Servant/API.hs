module Evoli.Assignment.Adapters.Servant.API where

import Evoli.Assignment.Model
import qualified Evoli.Assignment.UseCase as UC

import Polysemy
import Polysemy.Error (Error)
import Polysemy.Log (Log)
import Servant

type ApplicantCapt =
  Capture' '[Description "The name of an applicant, should be non-empty"] "app" Applicant

type AssignmentAPI =
        "quote"     :> Summary "Generates an insurance quote"
                    :> ApplicantCapt
                    :> ReqBody  '[JSON] (Insurance Quote)
                    :> Post     '[JSON] InsurancePrice

  :<|>  "quote"     :> Summary "Discards an existing quote from given applicant"
                    :> ApplicantCapt
                    :> Delete   '[JSON] ()

  :<|>  "policy"    :> Summary "Accepts an existing quote as a policy of given applicant"
                    :> ApplicantCapt
                    :> Post     '[JSON] ()

  :<|>  "policy"    :> Summary "Gets an accepted policy of given applicant"
                    :> ApplicantCapt
                    :> Get      '[JSON] (Insurance Policy)

assignmentServer
  :: ( UC.InsuranceStor Quote `Member` r
     , UC.InsuranceStor Policy `Member` r
     , Error UC.UseCaseError `Member` r
     , Log `Member` r
     )
  => ServerT AssignmentAPI (Sem r)
assignmentServer =
       UC.createQuote
  :<|> UC.discardQuote
  :<|> UC.acceptQuote
  :<|> UC.getPolicy

assignmentAPI :: Proxy AssignmentAPI
assignmentAPI = Proxy
