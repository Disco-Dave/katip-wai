-- | Add information about the 'Network.Wai.Request', 'Network.Wai.Response',
-- and the response time to Katip's 'Katip.LogContexts'.
--
-- TODO Add examples
module Katip.Wai
  ( -- * Middleware

    -- | 'Network.Wai.Middleware' for logging request and response information.
    Middleware.middleware
  , Middleware.middlewareCustom

    -- ** Helpers

    -- | Since logging with @Katip@ is monadic, we need the ability to run a
    -- 'Network.Wai.Application' or 'Network.Wai.Middleware' in a monad other
    -- than @IO@.
  , Middleware.ApplicationT
  , Middleware.MiddlewareT
  , Middleware.runApplication

    -- * Options

    -- | Options for customizing the way 'Middleware.middlewareCustom' handles the requests and responses.
    --
    -- TODO Add examples
  , Options.Options (..)
  , Options.addRequestAndResponseToContext
  , Options.logRequestAndResponse
  , Options.options
  , Options.defaultOptions

    -- ** Formatting

    -- | Functions for formatting the 'Request.Request's and 'Response.Response's.
  , Options.Formatter
  , Options.TimeUnit (..)
  , Options.IncludedHeaders
  , Options.defaultIncludedHeaders
  , Options.defaultRequestFormat
  , Options.defaultResponseFormat

    -- * Request
  , Request.Request (..)
  , Request.traceRequest

    -- * Response
  , Response.Response (..)
  , Response.traceResponse
  )
where

import qualified Katip.Wai.Middleware as Middleware
import qualified Katip.Wai.Options as Options
import qualified Katip.Wai.Request as Request
import qualified Katip.Wai.Response as Response

