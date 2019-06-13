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

import Data.ByteString.Conversion (List(..))
import Data.LanguageCodes
import Data.Text.Ascii
import Data.ISO3166_CountryCodes
import Data.Currency (Alpha)
import Galley.Types.Teams
import Galley.Types.Bot.Service
import Brig.Types.Client.Prekey (PrekeyId, Prekey, LastPrekey)
import Brig.Types.Provider
import Brig.Types.Connection
import Brig.Types.User
import Brig.Types.Intra
import qualified Data.Metrics as Metrics
import Control.Lens
import Data.Range
import Data.Aeson (toJSON)
import Brig.Types.User.Auth (CookieLabel)
import Data.Aeson (Value(..))
import Data.HashMap.Strict.InsOrd
import Data.Id
import Data.Misc
import Data.Proxy
import Data.Text as Text (unlines)
import Data.UUID (UUID, fromText)
import Servant hiding (Get, Put, Post, Delete, ReqBody)
import qualified Servant
import Servant.Swagger
import URI.ByteString.QQ (uri)
import qualified Data.Json.Util

import Brig.Types.Activation




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
      :> QueryParam' '[Optional] "filter" Relation
      :> QueryParam "users" UserId
      :> Get [ConnectionStatus]
     -- handler: deprecatedGetConnectionsStatus

  :<|> "users" :> "connections-status"
      :> QueryParam' '[Optional] "filter" Relation
      :> ReqBody ConnectionsStatusRequest
      :> Post [ConnectionStatus]
     -- handler: getConnectionsStatus

  :<|> "users"
      :> QueryParam "ids" (List UserId)
      :> Get _
     -- handler: listActivatedAccounts

  :<|> "users"
      :> QueryParam "handles" (List Handles)
      :> Get _
     -- handler: listActivatedAccounts



{-

    get "/i/users" (continue listAccountsByIdentity) $
        accept "application" "json"
        .&. (param "email" ||| param "phone")

    put "/i/users/:id/status" (continue changeAccountStatus) $
        capture "id"
        .&. jsonRequest @AccountStatusUpdate

    get "/i/users/:id/status" (continue getAccountStatus) $
        accept "application" "json"
        .&. capture "id"

    get "/i/users/:id/contacts" (continue getContactList) $
        accept "application" "json"
        .&. capture "id"

    get "/i/users/activation-code" (continue getActivationCode) $
        accept "application" "json"
        .&. (param "email" ||| param "phone")

    get "/i/users/password-reset-code" (continue getPasswordResetCode) $
        accept "application" "json"
        .&. (param "email" ||| param "phone")

    post "/i/users/revoke-identity" (continue revokeIdentity) $
        param "email" ||| param "phone"

    head "/i/users/blacklist" (continue checkBlacklist) $
        param "email" ||| param "phone"

    delete "/i/users/blacklist" (continue deleteFromBlacklist) $
        param "email" ||| param "phone"

    post "/i/users/blacklist" (continue addBlacklist) $
        param "email" ||| param "phone"

    -- given a phone number (or phone number prefix), see whether
    -- it is blocked via a prefix (and if so, via which specific prefix)
    get "/i/users/phone-prefixes/:prefix" (continue getPhonePrefixes) $
        capture "prefix"

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










instance ToSchema HttpsUrl where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToSchema ServiceKeyPEM where
    declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @Text)
      where
        tweak = fmap $ schema . example ?~ pem
        pem = String . Text.unlines $
            [ "-----BEGIN PUBLIC KEY-----"
            , "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0"
            , "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH"
            , "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV"
            , "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS"
            , "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8"
            , "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la"
            , "nQIDAQAB"
            , "-----END PUBLIC KEY-----"
            ]

instance ToSchema (Fingerprint Rsa) where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)  -- TODO (at least inject a plausible example)

instance ToSchema ServiceToken where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)  -- TODO (at least inject a plausible example)

instance ToSchema NewLegalHoldService where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "newLegalHoldServiceKey"   -> "public_key"
              "newLegalHoldServiceUrl"   -> "base_url"
              "newLegalHoldServiceToken" -> "auth_token"
          }

instance ToSchema ViewLegalHoldService where
    declareNamedSchema _ = pure $ NamedSchema (Just "ViewLegalHoldService") $ mempty
        & properties .~ properties_
        & example .~ example_
        & required .~ ["status"]
        & minProperties .~ Just 1
        & maxProperties .~ Just 2
        & type_ .~ SwaggerObject
      where
        properties_ :: InsOrdHashMap Text (Referenced Schema)
        properties_ = fromList
          [ ("status", Inline (toSchema (Proxy @MockViewLegalHoldServiceStatus)))
          , ("info", Inline (toSchema (Proxy @ViewLegalHoldServiceInfo)))
          ]

        example_ :: Maybe Value
        example_ = Just . toJSON
                 $ ViewLegalHoldService (ViewLegalHoldServiceInfo (Id tid) lhuri fpr)
          where
            Just tid = fromText "7fff70c6-7b9c-11e9-9fbd-f3cc32e6bbec"
            Right lhuri = mkHttpsUrl [uri|https://example.com/|]
            fpr = Fingerprint "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"

-- | this type is only introduce locally here to generate the schema for 'ViewLegalHoldService'.
data MockViewLegalHoldServiceStatus = Configured | NotConfigured | Disabled
  deriving (Eq, Show, Generic)

instance ToSchema MockViewLegalHoldServiceStatus where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions { constructorTagModifier = camelToUnderscore }

instance ToSchema ViewLegalHoldServiceInfo where
    {-

    -- FUTUREWORK: The generic instance uses a reference to the UUID type in TeamId.  This
    -- leads to perfectly valid swagger output, but 'validateEveryToJSON' chokes on it
    -- (unknown schema "UUID").  In order to be able to run those tests, we construct the
    -- 'ToSchema' instance manually.
    -- See also: https://github.com/haskell-servant/servant-swagger/pull/104

    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "viewLegalHoldServiceFingerprint" -> "fingerprint"
              "viewLegalHoldServiceUrl"         -> "base_url"
              "viewLegalHoldServiceTeam"        -> "team_id"
          }
    -}
    declareNamedSchema _ = pure $ NamedSchema (Just "ViewLegalHoldServiceInfo") $ mempty
        & properties .~ properties_
        & example .~ example_
        & required .~ ["team_id", "base_url", "fingerprint"]
        & type_ .~ SwaggerObject
      where
        properties_ :: InsOrdHashMap Text (Referenced Schema)
        properties_ = fromList
          [ ("team_id", Inline (toSchema (Proxy @UUID)))
          , ("base_url", Inline (toSchema (Proxy @HttpsUrl)))
          , ("fingerprint", Inline (toSchema (Proxy @(Fingerprint Rsa))))
          ]

        example_ :: Maybe Value
        example_ = Just . toJSON
                 $ ViewLegalHoldServiceInfo (Id tid) lhuri fpr
          where
            Just tid = fromText "7fff70c6-7b9c-11e9-9fbd-f3cc32e6bbec"
            Right lhuri = mkHttpsUrl [uri|https://example.com/|]
            fpr = Fingerprint "\138\140\183\EM\226#\129\EOTl\161\183\246\DLE\161\142\220\239&\171\241h|\\GF\172\180O\129\DC1!\159"

instance ToSchema LegalHoldTeamConfig where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "legalHoldTeamConfigStatus" -> "status"
          }

instance ToSchema LegalHoldStatus where
    declareNamedSchema = tweak . genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { constructorTagModifier = \case
              "LegalHoldDisabled" -> "disabled"
              "LegalHoldEnabled"  -> "enabled"
          }

        tweak = fmap $ schema . description ?~ descr
          where
            descr = "determines whether admins of a team " <>
                    "are allowed to enable LH for their users."

instance ToSchema RequestNewLegalHoldClient where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "userId" -> "user_id"
              "teamId" -> "team_id"
          }

instance ToSchema NewLegalHoldClient where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "newLegalHoldClientPrekeys"     -> "prekeys"
              "newLegalHoldClientLastKey"     -> "last_prekey"
          }

instance ToSchema UserLegalHoldStatusResponse where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "ulhsrStatus" -> "status"
              "ulhsrLastPrekey" -> "last_prekey"
              "ulhsrClientId" -> "client_id"
          }

instance ToSchema UserLegalHoldStatus where
    declareNamedSchema = tweak . genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { constructorTagModifier = \case
              "UserLegalHoldEnabled"  -> "enabled"
              "UserLegalHoldPending"  -> "pending"
              "UserLegalHoldDisabled" -> "disabled"
          }

        tweak = fmap $ schema . description ?~ descr
          where
            descr = "states whether a user is under legal hold, " <>
                    "or whether legal hold is pending approval."

instance ToSchema ClientId where
    declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @Text)
      where
        tweak = fmap $ schema . description ?~ descr
          where
            descr = "A Client Id"

instance ToSchema PrekeyId where
    declareNamedSchema _ = tweak $ declareNamedSchema (Proxy @Int)
      where
        tweak = fmap $ schema . description ?~ descr
          where
            descr = "in the range [0..65535]."

instance ToSchema Prekey where
    declareNamedSchema = genericDeclareNamedSchema opts
      where
        opts = defaultSchemaOptions
          { fieldLabelModifier = \case
              "prekeyId" -> "id"
              "prekeyKey" -> "key"
          }

instance ToSchema LastPrekey where
    declareNamedSchema _ = declareNamedSchema (Proxy @Prekey)

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
type Delete202 = Verb 'DELETE 202 '[JSON]

type ReqBody = Servant.ReqBody '[JSON]





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

instance ToParamSchema a => ToParamSchema (List a) where
    toParamSchema = undefined

instance ToParamSchema Handle where




-- TODO: read ~/src/wire-server-swaggrify/libs/brig-types/src/Brig/Types/Swagger.hs and see what we've missed.



-- data Auth creds where
  -- @... :> Auth '["zuser", "zconn"] :> ...@
  -- @... :> Auth '["zuser"] :> ...@
  -- @... :> Auth '["zbot"] :> ...@
  -- ...

    -- handler instances can then be written in terms of the expanded type aliases (@.. :> Header ... :> ...@).
