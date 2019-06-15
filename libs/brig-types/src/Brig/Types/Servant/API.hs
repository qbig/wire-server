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
import Galley.Types
import Galley.Types.Bot.Service
import Galley.Types.Teams
import qualified Data.Json.Util
import qualified Data.Metrics as Metrics
import qualified Servant
import Servant hiding (Get, Put, Post, Delete, ReqBody, QueryParam, QueryParam')
import Servant.Swagger
import Servant.API.Generic
import Servant.Server.Generic
import URI.ByteString.QQ (uri)

import Brig.Types.Servant.Orphans
import qualified Brig.Types.Servant.API.Internal
import qualified Brig.Types.Servant.API.Users



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



--
----------------------------------------------------------------------


swagger :: Swagger
swagger = toSwagger api

api :: Proxy (ToServantApi API')
api = genericApi (Proxy :: Proxy API')

data API route = API
  { _route_i     :: route :- "i"     :> ToServantApi Brig.Types.Servant.API.Internal.API
  , _route_users :: route :- "users" :> ToServantApi Brig.Types.Servant.API.Users.API
  }



-- TODO: read ~/src/wire-server-swaggrify/libs/brig-types/src/Brig/Types/Swagger.hs and see what we've missed.



----------------------------------------------------------------------
-- noise: this is how we'll build servers later.

data API' route = API'
  { _r0002 :: route :- Get NoContent
  }
  deriving (Generic)

server' :: API' AsServer
server' = API' { _r0002 = undefined :: Handler NoContent }

app' :: Application
app' = genericServe server'
