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
import Servant.API.Generic


type API = "i" :> Get NoContent

api :: Proxy (ToServantApi API')
api = genericApi (Proxy :: Proxy API')

-- $msg=sprintf("_r%4.4i", $i); if (/_r\d\d\d\d/) { $i++; s/_r000\d/$msg/e; } print
data API' route = API
  { _r0002 :: route :- "status"     :> Get NoContent
  , _r0003 :: route :- "status"     :> Head NoContent
  , _r0004 :: route :- "monitoring" :> Get Metrics.Metrics
  , _r0005 :: route :- "users" :> Capture "uid" UserId :> "auto-connect"
      :> InternalZConn
      :> ReqBody UserSet
      :> Post [UserConnection]
     -- handler: autoConnect

  , _r0011 :: route :- "users"
      :> ReqBody NewUser
      :> Post (Headers '[Servant.Header "Location" UserId] SelfProfile)
     -- handler: createUserNoVerify

  , _r0018 :: route :- "self" :> "email"
      :> InternalZUser
      :> ReqBody Brig.Types.User.EmailUpdate
      :> Put204 NoContent
      -- TODO: responds with 202 or 204 (decided dynamically).  can servant express that?
     -- handler: changeSelfEmailNoSend

  , _r0023 :: route :- "users" :> Capture "uid" UserId
      :> Delete202 NoContent
     -- handler: deleteUserNoVerify

  , _r0027 :: route :- "users" :> "connections-status"
      :> QueryParamOptional "filter" Relation
      :> QueryParamStrict "users" UserId
      :> Get [ConnectionStatus]
     -- handler: deprecatedGetConnectionsStatus

  , _r0033 :: route :- "users" :> "connections-status"
      :> QueryParamOptional "filter" Relation
      :> ReqBody ConnectionsStatusRequest
      :> Post [ConnectionStatus]
     -- handler: getConnectionsStatus

  , _r0039 :: route :- "users"
      :> QueryParamStrict "ids" (List UserId)
      :> Get [UserAccount]
     -- handler: listActivatedAccounts

  , _r0044 :: route :- "users"
      :> QueryParamStrict "handles" (List Handle)
      :> Get [UserAccount]
     -- handler: listActivatedAccounts

  , _r0049 :: route :- "users"
      :> QueryParamStrict "email" Email
      :> Get [UserAccount]
     -- handler: listAccountsByIdentity

  , _r0054 :: route :- "users"
      :> QueryParamStrict "phone" Phone
      :> Get [UserAccount]
     -- handler: listAccountsByIdentity

  , _r0059 :: route :- "users" :> Capture "uid" UserId :> "status"
      :> ReqBody AccountStatusUpdate
      :> Put200 NoContent
     -- handler: changeAccountStatus

  , _r0064 :: route :- "users" :> Capture "uid" UserId :> "status"
      :> Get AccountStatusObject
     -- handler: getAccountStatus

  , _r0068 :: route :- "users" :> Capture "uid" UserId :> "contacts"
      :> Get UserIds
     -- handler: getContactList

  , _r0072 :: route :- "users" :> "activation-code"
      :> QueryParamStrict "email" Email
      :> Get ActivationCodeObject
     -- handler: getActivationCode

  , _r0077 :: route :- "users" :> "activation-code"
      :> QueryParamStrict "phone" Phone
      :> Get ActivationCodeObject
     -- handler: getActivationCode

  , _r0082 :: route :- "users" :> "password-reset-code"
      :> QueryParamStrict "email" Email
      :> Get ActivationCodeObject
     -- handler: getPasswordResetCode

  , _r0087 :: route :- "users" :> "password-reset-code"
      :> QueryParamStrict "phone" Phone
      :> Get ActivationCodeObject
     -- handler: getPasswordResetCode

  , _r0092 :: route :- "users" :> "revoke-identity"
      :> QueryParamStrict "email" Email
      :> Get NoContent
     -- handler: revokeIdentity

  , _r0097 :: route :- "users" :> "revoke-identity"
      :> QueryParamStrict "phone" Phone
      :> Get NoContent
     -- handler: revokeIdentity

  , _r0102 :: route :- "users" :> "blacklist"
      :> QueryParamStrict "email" Email
      :> Get NoContent
     -- handler: checkBlacklist

  , _r0107 :: route :- "users" :> "blacklist"
      :> QueryParamStrict "phone" Phone
      :> Get NoContent
     -- handler: checkBlacklist

  , _r0112 :: route :- "users" :> "blacklist"
      :> QueryParamStrict "email" Email
      :> Delete200 NoContent
     -- handler: deleteFromBlacklist

  , _r0117 :: route :- "users" :> "blacklist"
      :> QueryParamStrict "phone" Phone
      :> Delete200 NoContent
     -- handler: deleteFromBlacklist

  , _r0122 :: route :- "users" :> "blacklist"
      :> QueryParamStrict "email" Email
      :> Post NoContent
     -- handler: addBlacklist

  , _r0127 :: route :- "users" :> "blacklist"
      :> QueryParamStrict "phone" Phone
      :> Post NoContent
     -- handler: addBlacklist

    -- given a phone number (or phone number prefix), see whether
    -- it is blocked via a prefix (and if so, via which specific prefix)
  , _r0134 :: route :- "users" :> "phone-prefixes" :> Capture "prefix" PhonePrefix
      :> Get [ExcludedPrefix]
     -- handler: getPhonePrefixes

  , _r0138 :: route :- "users" :> "phone-prefixes" :> Capture "prefix" PhonePrefix
      :> Delete200 NoContent
     -- handler: deleteFromPhonePrefix

  , _r0142 :: route :- "users" :> "phone-prefixes"
      :> ReqBody ExcludedPrefix
      :> Post NoContent
     -- handler: addPhonePrefix

    -- is :uid not team owner, or there are other team owners?
  , _r0148 :: route :- "users" :> Capture "uid" UserId :> "can-be-deleted" :> Capture "tid" TeamId
      :> Get NoContent  -- info is encoded in dynamic response status code
     -- handler: canBeDeleted

    -- is :uid team owner (the only one or one of several)?
  , _r0153 :: route :- "users" :> Capture "uid" UserId :> "is-team-owner" :> Capture "tid" TeamId
      :> Get NoContent  -- info is encoded in dynamic response status code
     -- handler: isTeamOwner

  , _r0157 :: route :- "users" :> Capture "uid" UserId :> "sso-id"
      :> ReqBody UserSSOId
      :> Put200 NoContent
     -- handler: updateSSOId

  , _r0162 :: route :- "users" :> Capture "uid" UserId :> "managed-by"
      :> ReqBody ManagedByUpdate
      :> Put200 NoContent
     -- handler: updateManagedBy

  , _r0167 :: route :- "users" :> Capture "uid" UserId :> "rich-info"
      :> ReqBody RichInfoUpdate
      :> Put200 NoContent
     -- handler: updateRichInfo

  , _r0172 :: route :- "clients"
      :> ReqBody UserSet
      :> Post UserClients
     -- handler: internalListClients
  }
  deriving (Generic)
