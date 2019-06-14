module Brig.Types.Servant.API.Internal where

import Brig.Types.Activation
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Servant.Orphans
import Brig.Types.User
import Data.ByteString.Conversion (List(..))
import Data.Id
import Galley.Types
import qualified Data.Metrics as Metrics
import qualified Servant
import Servant hiding (Get, Put, Post, Delete, ReqBody, QueryParam, QueryParam')


type InternalAPI
     = "status"     :> Get NoContent
  :<|> "status"     :> Head NoContent
  :<|> "monitoring" :> Get Metrics.Metrics

  :<|> "users" :> Capture "uid" UserId :> "auto-connect"
      -- TODO: opt (header "Z-Connection")
      :> ReqBody UserSet
      :> Post [UserConnection]
     -- handler: autoConnect

  :<|> "users"
      :> ReqBody NewUser
      :> Post (Headers '[Servant.Header "Location" UserId] SelfProfile)
     -- handler: createUserNoVerify

  :<|> "self" :> "email"
      -- TODO: header "Z-User"
      :> ReqBody Brig.Types.User.EmailUpdate
      :> Put204 NoContent
      -- TODO: responds with 202 or 204 (decided dynamically).  can servant express that?
     -- handler: changeSelfEmailNoSend

  :<|> "users" :> Capture "uid" UserId
      :> Delete202 NoContent
     -- handler: deleteUserNoVerify

  :<|> "users" :> "connections-status"
      :> QueryParamOptional "filter" Relation
      :> QueryParamStrict "users" UserId
      :> Get [ConnectionStatus]
     -- handler: deprecatedGetConnectionsStatus

  :<|> "users" :> "connections-status"
      :> QueryParamOptional "filter" Relation
      :> ReqBody ConnectionsStatusRequest
      :> Post [ConnectionStatus]
     -- handler: getConnectionsStatus

  :<|> "users"
      :> QueryParamStrict "ids" (List UserId)
      :> Get [UserAccount]
     -- handler: listActivatedAccounts

  :<|> "users"
      :> QueryParamStrict "handles" (List Handle)
      :> Get [UserAccount]
     -- handler: listActivatedAccounts

  :<|> "users"
      :> QueryParamStrict "email" Email
      :> Get [UserAccount]
     -- handler: listAccountsByIdentity

  :<|> "users"
      :> QueryParamStrict "phone" Phone
      :> Get [UserAccount]
     -- handler: listAccountsByIdentity

  :<|> "users" :> Capture "uid" UserId :> "status"
      :> ReqBody AccountStatusUpdate
      :> Put200 NoContent
     -- handler: changeAccountStatus

  :<|> "users" :> Capture "uid" UserId :> "status"
      :> Get AccountStatusObject
     -- handler: getAccountStatus

  :<|> "users" :> Capture "uid" UserId :> "contacts"
      :> Get UserIds
     -- handler: getContactList

  :<|> "users" :> "activation-code"
      :> QueryParamStrict "email" Email
      :> Get ActivationCodeObject
     -- handler: getActivationCode

  :<|> "users" :> "activation-code"
      :> QueryParamStrict "phone" Phone
      :> Get ActivationCodeObject
     -- handler: getActivationCode

  :<|> "users" :> "password-reset-code"
      :> QueryParamStrict "email" Email
      :> Get ActivationCodeObject
     -- handler: getPasswordResetCode

  :<|> "users" :> "password-reset-code"
      :> QueryParamStrict "phone" Phone
      :> Get ActivationCodeObject
     -- handler: getPasswordResetCode

  :<|> "users" :> "revoke-identity"
      :> QueryParamStrict "email" Email
      :> Get NoContent
     -- handler: revokeIdentity

  :<|> "users" :> "revoke-identity"
      :> QueryParamStrict "phone" Phone
      :> Get NoContent
     -- handler: revokeIdentity

  :<|> "users" :> "blacklist"
      :> QueryParamStrict "email" Email
      :> Get NoContent
     -- handler: checkBlacklist

  :<|> "users" :> "blacklist"
      :> QueryParamStrict "phone" Phone
      :> Get NoContent
     -- handler: checkBlacklist

  :<|> "users" :> "blacklist"
      :> QueryParamStrict "email" Email
      :> Delete200 NoContent
     -- handler: deleteFromBlacklist

  :<|> "users" :> "blacklist"
      :> QueryParamStrict "phone" Phone
      :> Delete200 NoContent
     -- handler: deleteFromBlacklist

  :<|> "users" :> "blacklist"
      :> QueryParamStrict "email" Email
      :> Post NoContent
     -- handler: addBlacklist

  :<|> "users" :> "blacklist"
      :> QueryParamStrict "phone" Phone
      :> Post NoContent
     -- handler: addBlacklist

    -- given a phone number (or phone number prefix), see whether
    -- it is blocked via a prefix (and if so, via which specific prefix)
  :<|> "users" :> "phone-prefixes" :> Capture "prefix" PhonePrefix
      :> Get [ExcludedPrefix]
     -- handler: getPhonePrefixes

  :<|> "users" :> "phone-prefixes" :> Capture "prefix" PhonePrefix
      :> Delete200 NoContent
     -- handler: deleteFromPhonePrefix

  :<|> "users" :> "phone-prefixes"
      :> ReqBody ExcludedPrefix
      :> Post NoContent
     -- handler: addPhonePrefix

    -- is :uid not team owner, or there are other team owners?
  :<|> "users" :> Capture "uid" UserId :> "can-be-deleted" :> Capture "tid" TeamId
      :> Get NoContent  -- info is encoded in dynamic response status code
     -- handler: canBeDeleted

    -- is :uid team owner (the only one or one of several)?
  :<|> "users" :> Capture "uid" UserId :> "is-team-owner" :> Capture "tid" TeamId
      :> Get NoContent  -- info is encoded in dynamic response status code
     -- handler: isTeamOwner

  :<|> "users" :> Capture "uid" UserId :> "sso-id"
      :> ReqBody UserSSOId
      :> Put200 NoContent
     -- handler: updateSSOId

  :<|> "users" :> Capture "uid" UserId :> "managed-by"
      :> ReqBody ManagedByUpdate
      :> Put200 NoContent
     -- handler: updateManagedBy

  :<|> "users" :> Capture "uid" UserId :> "rich-info"
      :> ReqBody RichInfoUpdate
      :> Put200 NoContent
     -- handler: updateRichInfo

  :<|> "clients"
      :> ReqBody UserSet
      :> Post UserClients
     -- handler: internalListClients
