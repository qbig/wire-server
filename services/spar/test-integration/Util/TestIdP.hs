{-# LANGUAGE RecordWildCards #-}

-- | TODO: consolidate this and the similar code in
-- @/services/brig/test/integration/API/Provider.hs@ to forecastle.
-- see https://github.com/wireapp/wire-server/pull/721
module Util.TestIdP where

import Imports hiding (head)
import Bilge
import Control.Concurrent.Chan
import Control.Monad.Catch
import Data.Aeson
import Data.String.Conversions (ConvertibleStrings, LBS)
import Network.Wai (Application)
import Util

import qualified Control.Concurrent.Async          as Async
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import qualified Network.Wai.Handler.WarpTLS       as Warp


data Config = Config
    { privateKey   :: FilePath
    , publicKey    :: FilePath
    , cert         :: FilePath
    , idpHost      :: Text
    , idpPort      :: Int
    } deriving (Show, Generic)

instance FromJSON Config

-- | Get the config from environment variables (and some defaults)
getEnvConfig :: IO Config
getEnvConfig = do
    privateKey <- getEnv "TEST_KEY"
    publicKey  <- getEnv "TEST_PUBKEY"
    cert       <- getEnv "TEST_CERT"
    let idpHost = "https://localhost"
    let idpPort = 9000
    pure Config{..}


{-
-- | Run a test case with an external service application.
withTestService
    :: Config
    -> DB.ClientState
    -> Brig
    -> (Chan e -> Application)
    -> (ServiceRef -> Chan e -> Http a)
    -> Http a
withTestService config db brig mkApp go = do
    sref <- registerService config db brig
    runService config mkApp (go sref)
-}

{-
registerService :: Config -> DB.ClientState -> Brig -> Http ServiceRef
registerService config db brig = do
    prv <- randomProvider db brig
    new <- defNewService config
    let Just url = fromByteString $
          encodeUtf8 (botHost config) <> ":" <>
          C8.pack (show (botPort config))
    svc <- addGetService brig (providerId prv) (new { newServiceUrl = url })
    let pid = providerId prv
    let sid = serviceId svc
    enableService brig pid sid
    return (newServiceRef sid pid)
-}

runService
    :: Config
    -> (Chan e -> Application)
    -> (Chan e -> Http a)
    -> Http a
runService config mkApp go = do
    let tlss = Warp.tlsSettings (cert config) (privateKey config)
    let defs = Warp.defaultSettings { Warp.settingsPort = idpPort config }
    buf <- liftIO newChan
    srv <- liftIO . Async.async $
        Warp.runTLS tlss defs $
            mkApp buf
    go buf `finally` liftIO (Async.cancel srv)


withServeViaHttps :: (ConvertibleStrings s LBS) => s -> ReaderT TestEnv IO ()
withServeViaHttps = undefined
