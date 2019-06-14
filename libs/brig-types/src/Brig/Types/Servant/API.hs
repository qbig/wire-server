{-# OPTIONS_GHC -Wno-unused-imports #-}



{-# OPTIONS_GHC -Wno-orphans #-}
-- FUTUREWORK: move the 'ToSchema' instances to their home modules (where the data types
-- live), and turn warning about orphans back on.

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- it's ok to not warn about unmatched patterns; 'validateEveryToJSON' will crash on them
-- before too long.

-- | Import this qualified.
module Brig.Types.Servant.API where

import Imports

import "swagger2" Data.Swagger hiding (Header(..))
  -- NB: this package depends on both types-common, swagger2, so there is no away around this name
  -- clash other than -XPackageImports.

import Brig.Types.Activation
import Brig.Types.Client.Prekey (PrekeyId, Prekey, LastPrekey)
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.Types.Provider
import Brig.Types.User
import Brig.Types.User.Auth (CookieLabel)
import Control.Lens
import Data.Aeson as Aeson
import Data.ByteString.Conversion (List(..))
import Data.Currency (Alpha)
import Data.HashMap.Strict.InsOrd
import Data.Id
import Data.ISO3166_CountryCodes
import Data.LanguageCodes
import Data.Misc
import Data.Proxy
import Data.Range
import Data.Text.Ascii
import Data.Text as Text (unlines)
import Data.UUID (UUID, fromText)
import Galley.Types.Bot.Service
import Galley.Types.Teams
import qualified Data.Json.Util
import qualified Data.Metrics as Metrics
import qualified Servant
import Servant hiding (Get, Put, Post, Delete, ReqBody, QueryParam, QueryParam')
import Servant.Swagger
import URI.ByteString.QQ (uri)




{-
import Data.String.Conversions
import System.Process (system)
import Data.Aeson (encode)
import Test.Hspec (hspec)
import Brig.Types.Test.Arbitrary ()

main :: IO ()
main = do
  writeFile "/tmp/x" . cs $ encode swagger
  void $ system "cat /tmp/x | json_pp && curl -X POST -d @/tmp/x -H 'Content-Type:application/json' http://online.swagger.io/validator/debug | json_pp"
  hspec $ validateEveryToJSON (Proxy @GalleyRoutes)
  -- see also: https://github.com/swagger-api/validator-badge

  -- alternatives:
  -- https://github.com/navidsh/maven.swagger.validator
  -- https://editor.swagger.io/  (this finds dangling refs.  good.)
  -- https://apidevtools.org/swagger-parser/online/  (also finds dangling refs, but it's *very slow*)
-}




----------------------------------------------------------------------
-- * more stuff we need to move to other places

newtype AccountStatusObject = AccountStatusObject AccountStatus
    deriving (Eq, Show, Generic)

instance FromJSON AccountStatusObject where
    parseJSON = withObject "account-status object" $
        \o -> AccountStatusObject <$> o .: "status"

instance ToJSON AccountStatusObject where
    toJSON (AccountStatusObject status) = object [ "status" Aeson..= status ]

data ActivationCodeObject = ActivationCodeObject ActivationKey ActivationCode
    deriving (Eq, Show, Generic)

instance FromJSON ActivationCodeObject where
    parseJSON = withObject "activation code object" $
        \o -> ActivationCodeObject <$> o .: "key" <*> o .: "code"

instance ToJSON ActivationCodeObject where
    toJSON (ActivationCodeObject key code) = object [ "key" Aeson..= key, "code" Aeson..= code ]


--
----------------------------------------------------------------------




swagger :: Swagger
swagger = toSwagger (Proxy @API)


type API = PublicAPI :<|> "i" :> InternalAPI

type PublicAPI = Get NoContent

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
  :<|> "users" :> "phone-prefixes" :> Capture "prefix" _
      :> Get _
     -- handler: getPhonePrefixes

    -- given a phone number (or phone number prefix), see whether
    -- it is blocked via a prefix (and if so, via which specific prefix)
    get "/i/users/phone-prefixes/:prefix" (continue ) $
        capture "prefix"





{-

    delete "/i/users/phone-prefixes/:prefix" (continue deleteFromPhonePrefix) $
        capture "prefix"

    post "/i/users/phone-prefixes" (continue addPhonePrefix) $
      accept "application" "json"
      .&. jsonRequest @ExcludedPrefix

    -- is :uid not team owner, or there are other team owners?
    get "/i/users/:uid/can-be-deleted/:tid" (continue canBeDeleted) $
      capture "uid"
      .&. capture "tid"

    -- is :uid team owner (the only one or one of several)?
    get "/i/users/:uid/is-team-owner/:tid" (continue isTeamOwner) $
      capture "uid"
      .&. capture "tid"

    put "/i/users/:uid/sso-id" (continue updateSSOId) $
      capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @UserSSOId

    put "/i/users/:uid/managed-by" (continue updateManagedBy) $
      capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @ManagedByUpdate

    put "/i/users/:uid/rich-info" (continue updateRichInfo) $
      capture "uid"
      .&. accept "application" "json"
      .&. jsonRequest @RichInfoUpdate

    post "/i/clients" (continue internalListClients) $
      accept "application" "json"
      .&. jsonRequest @UserSet









-}


----------------------------------------------------------------------
-- helpers

camelToUnderscore :: String -> String
camelToUnderscore = concatMap go . (ix 0 %~ toLower)
  where go x = if isUpper x then "_" <> [toLower x] else [x]



type Head = Verb 'HEAD 204 '[JSON]  -- TODO: which status code is this?
type Get = Verb 'GET 200 '[JSON]
type Post = Verb 'POST 201 '[JSON]
type Put204 = Verb 'PUT 204 '[JSON]
type Put200 = Verb 'PUT 200 '[JSON]
type Delete200 = Verb 'DELETE 200 '[JSON]
type Delete202 = Verb 'DELETE 202 '[JSON]

type ReqBody = Servant.ReqBody '[JSON]

type QueryParamStrict = Servant.QueryParam  -- TODO: which one?
type QueryParamOptional = Servant.QueryParam  -- TODO: which one?



instance ToParamSchema (Id a) where
    toParamSchema _ = toParamSchema (Proxy @UUID)

instance ToSchema (Id a) where
    declareNamedSchema _ = declareNamedSchema (Proxy @UUID)

instance ToSchema Metrics.Metrics where
    declareNamedSchema = undefined

instance ToSchema UserSet where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema UserConnection where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Relation where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToParamSchema Relation where

instance ToSchema Message where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Data.Json.Util.UTCTimeMillis where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema NewUser where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema NewUserOrigin where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema (Range from to typ) where
    declareNamedSchema = undefined

instance ToSchema User where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema UserIdentity where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Email where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Phone where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Name where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema UserSSOId where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Pict where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Asset where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema AssetSize where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema ColourId where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Locale where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Language where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Country where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema SelfProfile where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema ActivationCode where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema CookieLabel where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema PlainTextPassword where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema ManagedBy where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema InvitationCode where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema (NewTeam ()) where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema NewTeamUser where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema ServiceRef where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema BindingNewTeamUser where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema BindingNewTeam where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Alpha where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema (AsciiText Base64Url) where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema CountryCode where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Value where
    declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema Handle where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

deriving instance Generic ISO639_1

instance ToSchema ISO639_1 where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema Brig.Types.User.EmailUpdate where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema ConnectionsStatusRequest where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema ConnectionStatus where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema UserAccount where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToSchema AccountStatus where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance ToParamSchema (List a) where
    toParamSchema _ = toParamSchema (Proxy @Text)
-- alternative:
-- instance (Generic a, ToParamSchema a, ToParamSchema a) => ToParamSchema (List a)
-- deriving instance Generic a => Generic (List a)

instance ToParamSchema Email where
    toParamSchema _ = toParamSchema (Proxy @Text)

instance ToParamSchema Phone
instance ToParamSchema Handle
instance ToParamSchema AccountStatus
instance ToSchema AccountStatusUpdate
instance ToSchema AccountStatusObject
instance ToSchema ActivationCodeObject
instance ToSchema UserIds
instance ToSchema ActivationKey



-- TODO: read ~/src/wire-server-swaggrify/libs/brig-types/src/Brig/Types/Swagger.hs and see what we've missed.



-- data Auth creds where
  -- @... :> Auth '["zuser", "zconn"] :> ...@
  -- @... :> Auth '["zuser"] :> ...@
  -- @... :> Auth '["zbot"] :> ...@
  -- ...

    -- handler instances can then be written in terms of the expanded type aliases (@.. :> Header ... :> ...@).
