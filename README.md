# katip-wai [![ci](https://github.com/Disco-Dave/katip-wai/actions/workflows/ci.yaml/badge.svg)](https://github.com/Disco-Dave/katip-wai/actions/workflows/ci.yaml)

[Middleware](https://hackage.haskell.org/package/wai-3.2.3/docs/Network-Wai.html#t:Middleware) for logging http request and response information through [Katip](https://hackage.haskell.org/package/katip). 

## Example using Servant ([./example](./example))
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.Proxy (Proxy (Proxy))
import qualified Katip
import Katip.Wai (ApplicationT, runApplication)
import qualified Katip.Wai
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant
import System.IO (stdout)

type Api = Servant.GetNoContent

server :: Servant.ServerT Api (Katip.KatipContextT Servant.Handler)
server = do
  Katip.logLocM Katip.InfoS "This message should also have the request context"
  pure Servant.NoContent

mkApplication :: ApplicationT (Katip.KatipContextT IO)
mkApplication = Katip.Wai.middleware Katip.InfoS $ \request send -> do
  logEnv <- Katip.getLogEnv
  context <- Katip.getKatipContext
  namespace <- Katip.getKatipNamespace

  let hoistedApp =
        let proxy = Proxy @Api
            toHandler = Katip.runKatipContextT logEnv context namespace
            hoistedServer = Servant.hoistServer proxy toHandler server
         in Servant.serve proxy hoistedServer

  withRunInIO $ \toIO -> hoistedApp request (toIO . send)

withLogEnv :: (Katip.LogEnv -> IO a) -> IO a
withLogEnv useLogEnv = do
  handleScribe <-
    Katip.mkHandleScribeWithFormatter
      Katip.bracketFormat
      (Katip.ColorLog True)
      stdout
      (Katip.permitItem minBound)
      Katip.V3

  let makeLogEnv =
        Katip.initLogEnv "example-app" "local-dev"
          >>= Katip.registerScribe "stdout" handleScribe Katip.defaultScribeSettings

  bracket makeLogEnv Katip.closeScribes useLogEnv

main :: IO ()
main = withLogEnv $ \logEnv ->
  let toIO = Katip.runKatipContextT logEnv () "main"
      app = runApplication toIO mkApplication
   in Warp.run 5555 app
```

## Example output
```
[2021-12-22 19:16:17][example-app.main][Info][compe][PID 559366][ThreadId 23][request.bodyLength:KnownLength 0][request.path:][request.remoteHost:127.0.0.1:40500][request.headers.range:null][request.headers.userAgent:curl/7.80.0][request.headers.host:localhost:5555][request.headers.referer:null][request.method:GET][request.Version:HTTP/1.1][request.isSecure:False][request.id:0d5e7c47-816f-402b-914a-9d6923b99508] Request received
[2021-12-22 19:16:17][example-app.main][Info][compe][PID 559366][ThreadId 23][request.bodyLength:KnownLength 0][request.path:][request.remoteHost:127.0.0.1:40500][request.headers.range:null][request.headers.userAgent:curl/7.80.0][request.headers.host:localhost:5555][request.headers.referer:null][request.method:GET][request.Version:HTTP/1.1][request.isSecure:False][request.id:0d5e7c47-816f-402b-914a-9d6923b99508][main:Main Main.hs:21:3] This message should also have the request context
[2021-12-22 19:16:17][example-app.main][Info][compe][PID 559366][ThreadId 23][response.status:204][response.elapsedTimeInNanoSeconds:205439][request.bodyLength:KnownLength 0][request.path:][request.remoteHost:127.0.0.1:40500][request.headers.range:null][request.headers.userAgent:curl/7.80.0][request.headers.host:localhost:5555][request.headers.referer:null][request.method:GET][request.Version:HTTP/1.1][request.isSecure:False][request.id:0d5e7c47-816f-402b-914a-9d6923b99508] Response sent
[2021-12-22 19:17:22][example-app.main][Info][compe][PID 560230][ThreadId 23][request.bodyLength:KnownLength 0][request.path:some/path][request.remoteHost:127.0.0.1:40502][request.headers.range:null][request.headers.userAgent:curl/7.80.0][request.headers.host:localhost:5555][request.headers.referer:null][request.method:GET][request.Version:HTTP/1.1][request.isSecure:False][request.id:b30f962e-32ff-4c05-9c5f-b60f487ea886] Request received
[2021-12-22 19:17:22][example-app.main][Info][compe][PID 560230][ThreadId 23][response.status:404][response.elapsedTimeInNanoSeconds:280937][request.bodyLength:KnownLength 0][request.path:some/path][request.remoteHost:127.0.0.1:40502][request.headers.range:null][request.headers.userAgent:curl/7.80.0][request.headers.host:localhost:5555][request.headers.referer:null][request.method:GET][request.Version:HTTP/1.1][request.isSecure:False][request.id:b30f962e-32ff-4c05-9c5f-b60f487ea886] Response sent

```
