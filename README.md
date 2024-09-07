# katip-wai [![ci](https://github.com/Disco-Dave/katip-wai/actions/workflows/ci.yaml/badge.svg)](https://github.com/Disco-Dave/katip-wai/actions/workflows/ci.yaml)

[Middleware](https://hackage.haskell.org/package/wai-3.2.3/docs/Network-Wai.html#t:Middleware) for logging http request and response information through [Katip](https://hackage.haskell.org/package/katip). 

You can find the full documentation on [Hackage](https://hackage.haskell.org/package/katip-wai/docs/Katip-Wai.html).

## Example using Servant ([./example](./example))
```haskell
import Control.Exception (bracket)
import Data.Proxy (Proxy (Proxy))
import Katip qualified
import Katip.Wai (ApplicationT, runApplication)
import Katip.Wai qualified
import Network.Wai.Handler.Warp qualified as Warp
import Servant qualified
import System.IO (stdout)
import UnliftIO (MonadUnliftIO (withRunInIO))


type Api = Servant.GetNoContent


server :: Servant.ServerT Api (Katip.KatipContextT Servant.Handler)
server = do
  Katip.logLocM Katip.InfoS "This message should also have the request context"
  pure Servant.NoContent


mkApplication :: ApplicationT (Katip.KatipContextT IO)
mkApplication = Katip.Wai.middleware Katip.InfoS $ request send -> do
  logEnv <- Katip.getLogEnv
  context <- Katip.getKatipContext
  namespace <- Katip.getKatipNamespace

  let hoistedApp =
        let proxy = Proxy @Api
            hoistedServer = Servant.hoistServer proxy (Katip.runKatipContextT logEnv context namespace) server
         in Servant.serve proxy hoistedServer

  withRunInIO $ toIO -> hoistedApp request (toIO . send)


withLogEnv :: (Katip.LogEnv -> IO a) -> IO a
withLogEnv useLogEnv = do
  handleScribe <-
    Katip.mkHandleScribeWithFormatter
      Katip.jsonFormat
      (Katip.ColorLog False)
      stdout
      (Katip.permitItem minBound)
      Katip.V3

  let makeLogEnv =
        Katip.initLogEnv "example-app" "local-dev"
          >>= Katip.registerScribe "stdout" handleScribe Katip.defaultScribeSettings

  bracket makeLogEnv Katip.closeScribes useLogEnv


main :: IO ()
main = withLogEnv $ logEnv ->
  let
    app = runApplication (Katip.runKatipContextT logEnv () "main") mkApplication
   in
    Warp.run 5555 app
```

## Example output
```
{"app":["example-app"],"at":"2024-09-07T18:44:10.411097829Z","data":{"request":{"headers":{Host:"localhost:5555","User-Agent":"curl8.9.1"},"httpVersion":"HTTP1.1","id":"7ec0fbc4-722c-4c70-a168-c2abe5c7b4fa","isSecure":false,"method":GET,"path":"/","queryString":[],"receivedAt":"2024-09-07T18:44:10.411057334Z","remoteHost":"127.0.0.1:51230"}},"env":"local-dev","host":"x1g11","loc":null,"msg":"Request received.","ns":["example-app","main"],"pid":"106249","sev":Info,"thread":"27"}
{"app":["example-app"],"at":"2024-09-07T18:44:10.411097829Z","data":{"request":{"headers":{Host:"localhost:5555","User-Agent":"curl8.9.1"},"httpVersion":"HTTP1.1","id":"7ec0fbc4-722c-4c70-a168-c2abe5c7b4fa","isSecure":false,"method":GET,"path":"","queryString":[],"receivedAt":"2024-09-07T18:44:10.411057334Z","remoteHost":"127.0.0.1:51230"}},"env":"local-dev","host":"x1g11","loc":{"loc_col":3,"loc_fn":"srcKatipWaiExample/Short.hs","loc_ln":19,"loc_mod":Katip.Wai.Example.Short,"loc_pkg":"my-katip-wai-example-0.1.0.0-inplace"},"msg":"This message should also have the request context","ns":["example-app","main"],"pid":"106249","sev":Info,"thread":"27"}
{"app":["example-app"],"at":"2024-09-07T18:44:10.411097829Z","data":{"request":{"headers":{Host:"localhost:5555","User-Agent":"curl8.9.1"},"httpVersion":"HTTP1.1","id":"7ec0fbc4-722c-4c70-a168-c2abe5c7b4fa","isSecure":false,"method":GET,"path":"/","queryString":[],"receivedAt":"2024-09-07T18:44:10.411057334Z","remoteHost":"127.0.0.1:51230"},"response":{"headers":{},"respondedAt":"2024-09-07T18:44:10.411199014Z","responseTime":{"time":0.137369,"unit":"ms"},"status":{"code":204,"message":"No Content"}}},"env":"local-dev","host":"x1g11","loc":null,"msg":"Response sent.","ns":["example-app","main"],"pid":"106249","sev":Info,"thread":"27"}
```
