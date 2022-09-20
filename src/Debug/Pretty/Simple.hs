{-# LANGUAGE CPP #-}

{-|
Module      : Debug.Pretty.Simple
Copyright   : (c) Dennis Gosnell, 2017
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the same functionality with Prelude's "Debug.Trace" module,
with pretty printing the debug strings.

Warning: This module also shares the same unsafety of "Debug.Trace" module.
-}

module Debug.Pretty.Simple
  ( -- * Trace with color on dark background
    -- This determines whether to print in color by looking at whether 'stderr'
    -- is a TTY device.
    pTrace
  , pTraceId
  , pTraceShow
  , pTraceShowId
  , pTraceIO
  , pTraceM
  , pTraceShowM
  , pTraceStack
  , pTraceEvent
  , pTraceEventIO
  , pTraceMarker
  , pTraceMarkerIO
  , pTraceWith
  , pTraceShowWith
    -- * Trace forcing color
  , pTraceForceColor
  , pTraceIdForceColor
  , pTraceShowForceColor
  , pTraceShowIdForceColor
  , pTraceMForceColor
  , pTraceShowMForceColor
  , pTraceStackForceColor
  , pTraceEventForceColor
  , pTraceEventIOForceColor
  , pTraceMarkerForceColor
  , pTraceMarkerIOForceColor
  , pTraceIOForceColor
    -- * Trace without color
  , pTraceNoColor
  , pTraceIdNoColor
  , pTraceShowNoColor
  , pTraceShowIdNoColor
  , pTraceMNoColor
  , pTraceShowMNoColor
  , pTraceStackNoColor
  , pTraceEventNoColor
  , pTraceEventIONoColor
  , pTraceMarkerNoColor
  , pTraceMarkerIONoColor
  , pTraceIONoColor
    -- * Trace With 'OutputOptions'
  , pTraceOpt
  , pTraceIdOpt
  , pTraceShowOpt
  , pTraceShowIdOpt
  , pTraceOptIO
  , pTraceOptM
  , pTraceShowOptM
  , pTraceStackOpt
  , pTraceEventOpt
  , pTraceEventOptIO
  , pTraceMarkerOpt
  , pTraceMarkerOptIO
  ) where

import Control.Monad ((<=<))
import Data.Text.Lazy (Text, unpack)
import Debug.Trace
       (trace, traceEvent, traceEventIO, traceIO, traceM, traceMarker,
        traceMarkerIO, traceStack)
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Pretty.Simple
       (CheckColorTty(..), OutputOptions, pStringOpt,
        defaultOutputOptionsNoColor, defaultOutputOptionsDarkBg)
import Text.Pretty.Simple.Internal (hCheckTTY)

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif

{-|
The 'pTraceIO' function outputs the trace message from the IO monad.
This sequences the output with respect to other IO actions.

@since 2.0.1.0
-}
{-# WARNING pTraceIO "'pTraceIO' remains in code" #-}
pTraceIO :: String -> IO ()
pTraceIO = pTraceOptIO CheckColorTty defaultOutputOptionsDarkBg

{-|
The 'pTrace' function pretty prints the trace message given as its first
argument, before returning the second argument as its result.

For example, this returns the value of @f x@ but first outputs the message.

> pTrace ("calling f with x = " ++ show x) (f x)

The 'pTrace' function should /only/ be used for debugging, or for monitoring
execution. The function is not referentially transparent: its type indicates
that it is a pure function but it has the side effect of outputting the
trace message.

@since 2.0.1.0
-}
{-# WARNING pTrace "'pTrace' remains in code" #-}
pTrace :: String -> a -> a
pTrace = pTraceOpt CheckColorTty defaultOutputOptionsDarkBg

{-|
Like 'pTrace' but returns the message instead of a third value.

@since 2.0.1.0
-}
{-# WARNING pTraceId "'pTraceId' remains in code" #-}
pTraceId :: String -> String
pTraceId = pTraceIdOpt CheckColorTty defaultOutputOptionsDarkBg

{-|
Like 'pTrace', but uses 'show' on the argument to convert it to a 'String'.

This makes it convenient for printing the values of interesting variables or
expressions inside a function. For example here we print the value of the
variables @x@ and @z@:

> f x y =
>     pTraceShow (x, z) $ result
>   where
>     z = ...
>     ...

@since 2.0.1.0
-}
{-# WARNING pTraceShow "'pTraceShow' remains in code" #-}
pTraceShow :: (Show a) => a -> b -> b
pTraceShow = pTraceShowOpt CheckColorTty defaultOutputOptionsDarkBg

{-|
Like 'pTraceShow' but returns the shown value instead of a third value.

@since 2.0.1.0
-}
{-# WARNING pTraceShowId "'pTraceShowId' remains in code" #-}
pTraceShowId :: (Show a) => a -> a
pTraceShowId = pTraceShowIdOpt CheckColorTty defaultOutputOptionsDarkBg
{-|
Like 'pTrace' but returning unit in an arbitrary 'Applicative' context. Allows
for convenient use in do-notation.

Note that the application of 'pTraceM' is not an action in the 'Applicative'
context, as 'pTraceIO' is in the 'IO' type. While the fresh bindings in the
following example will force the 'traceM' expressions to be reduced every time
the @do@-block is executed, @traceM "not crashed"@ would only be reduced once,
and the message would only be printed once.  If your monad is in 'MonadIO',
@liftIO . pTraceIO@ may be a better option.

> ... = do
>   x <- ...
>   pTraceM $ "x: " ++ show x
>   y <- ...
>   pTraceM $ "y: " ++ show y

@since 2.0.1.0
-}
{-# WARNING pTraceM "'pTraceM' remains in code" #-}
#if __GLASGOW_HASKELL__ < 800
pTraceM :: (Monad f) => String -> f ()
#else
pTraceM :: (Applicative f) => String -> f ()
#endif
pTraceM = pTraceOptM CheckColorTty defaultOutputOptionsDarkBg
{-|
Like 'pTraceM', but uses 'show' on the argument to convert it to a 'String'.

> ... = do
>   x <- ...
>   pTraceShowM $ x
>   y <- ...
>   pTraceShowM $ x + y

@since 2.0.1.0
-}
{-# WARNING pTraceShowM "'pTraceShowM' remains in code" #-}
#if __GLASGOW_HASKELL__ < 800
pTraceShowM :: (Show a, Monad f) => a -> f ()
#else
pTraceShowM :: (Show a, Applicative f) => a -> f ()
#endif
pTraceShowM = pTraceShowOptM CheckColorTty defaultOutputOptionsDarkBg

{-|
like 'pTrace', but additionally prints a call stack if one is
available.

In the current GHC implementation, the call stack is only
available if the program was compiled with @-prof@; otherwise
'pTraceStack' behaves exactly like 'pTrace'.  Entries in the call
stack correspond to @SCC@ annotations, so it is a good idea to use
@-fprof-auto@ or @-fprof-auto-calls@ to add SCC annotations automatically.

@since 2.0.1.0
-}
{-# WARNING pTraceStack "'pTraceStack' remains in code" #-}
pTraceStack :: String -> a -> a
pTraceStack = pTraceStackOpt CheckColorTty defaultOutputOptionsDarkBg

{-|
The 'pTraceEvent' function behaves like 'trace' with the difference that
the message is emitted to the eventlog, if eventlog profiling is available
and enabled at runtime.

It is suitable for use in pure code. In an IO context use 'pTraceEventIO'
instead.

Note that when using GHC's SMP runtime, it is possible (but rare) to get
duplicate events emitted if two CPUs simultaneously evaluate the same thunk
that uses 'pTraceEvent'.

@since 2.0.1.0
-}
{-# WARNING pTraceEvent "'pTraceEvent' remains in code" #-}
pTraceEvent :: String -> a -> a
pTraceEvent = pTraceEventOpt CheckColorTty defaultOutputOptionsDarkBg

{-|
The 'pTraceEventIO' function emits a message to the eventlog, if eventlog
profiling is available and enabled at runtime.

Compared to 'pTraceEvent', 'pTraceEventIO' sequences the event with respect to
other IO actions.

@since 2.0.1.0
-}
{-# WARNING pTraceEventIO "'pTraceEventIO' remains in code" #-}
pTraceEventIO :: String -> IO ()
pTraceEventIO = pTraceEventOptIO CheckColorTty defaultOutputOptionsDarkBg

-- | The 'pTraceMarker' function emits a marker to the eventlog, if eventlog
-- profiling is available and enabled at runtime. The @String@ is the name of
-- the marker. The name is just used in the profiling tools to help you keep
-- clear which marker is which.
--
-- This function is suitable for use in pure code. In an IO context use
-- 'pTraceMarkerIO' instead.
--
-- Note that when using GHC's SMP runtime, it is possible (but rare) to get
-- duplicate events emitted if two CPUs simultaneously evaluate the same thunk
-- that uses 'pTraceMarker'.
--
-- @since 2.0.1.0
{-# WARNING pTraceMarker "'pTraceMarker' remains in code" #-}
pTraceMarker :: String -> a -> a
pTraceMarker = pTraceMarkerOpt CheckColorTty defaultOutputOptionsDarkBg

-- | The 'pTraceMarkerIO' function emits a marker to the eventlog, if eventlog
-- profiling is available and enabled at runtime.
--
-- Compared to 'pTraceMarker', 'pTraceMarkerIO' sequences the event with respect
-- to other IO actions.
--
-- @since 2.0.1.0
{-# WARNING pTraceMarkerIO "'pTraceMarkerIO' remains in code" #-}
pTraceMarkerIO :: String -> IO ()
pTraceMarkerIO = pTraceMarkerOptIO CheckColorTty defaultOutputOptionsDarkBg

-- | The 'pTraceWith' function pretty prints the result of
-- applying @f to @a and returns back @a
--
-- @since ?
{-# WARNING pTraceWith "'pTraceWith' remains in code" #-}
pTraceWith :: (a -> String) -> a -> a
pTraceWith f a = pTrace (f a) a

-- | The 'pTraceShowWith' function similar to 'pTraceWith' except that
-- @f can return any type that implements Show
--
-- @since ?
{-# WARNING pTraceShowWith "'pTraceShowWith' remains in code" #-}
pTraceShowWith :: Show b => (a -> b) -> a -> a
pTraceShowWith f = (show . f) >>= pTraceShow

------------------------------------------
-- Helpers
------------------------------------------
{-# WARNING pStringTTYOptIO "'pStringTTYOptIO' remains in code" #-}
pStringTTYOptIO :: CheckColorTty -> OutputOptions -> String -> IO Text
pStringTTYOptIO checkColorTty outputOptions v = do
  realOutputOpts <-
    case checkColorTty of
      CheckColorTty -> hCheckTTY stderr outputOptions
      NoCheckColorTty -> pure outputOptions
  pure $ pStringOpt realOutputOpts v

{-# WARNING pStringTTYOpt "'pStringTTYOpt' remains in code" #-}
pStringTTYOpt :: CheckColorTty -> OutputOptions -> String -> Text
pStringTTYOpt checkColorTty outputOptions =
  unsafePerformIO . pStringTTYOptIO checkColorTty outputOptions

{-# WARNING pShowTTYOptIO "'pShowTTYOptIO' remains in code" #-}
pShowTTYOptIO :: Show a => CheckColorTty -> OutputOptions -> a -> IO Text
pShowTTYOptIO checkColorTty outputOptions =
  pStringTTYOptIO checkColorTty outputOptions . show

{-# WARNING pShowTTYOpt "'pShowTTYOpt' remains in code" #-}
pShowTTYOpt :: Show a => CheckColorTty -> OutputOptions -> a -> Text
pShowTTYOpt checkColorTty outputOptions =
  unsafePerformIO . pShowTTYOptIO checkColorTty outputOptions

------------------------------------------
-- Traces forcing color
------------------------------------------
-- | Similar to 'pTrace', but forcing color.
{-# WARNING pTraceForceColor "'pTraceForceColor' remains in code" #-}
pTraceForceColor :: String -> a -> a
pTraceForceColor = pTraceOpt NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pTraceId', but forcing color.
{-# WARNING pTraceIdForceColor "'pTraceIdForceColor' remains in code" #-}
pTraceIdForceColor :: String -> String
pTraceIdForceColor = pTraceIdOpt NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pTraceShow', but forcing color.
{-# WARNING pTraceShowForceColor "'pTraceShowForceColor' remains in code" #-}
pTraceShowForceColor :: (Show a) => a -> b -> b
pTraceShowForceColor = pTraceShowOpt NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pTraceShowId', but forcing color.
{-# WARNING pTraceShowIdForceColor "'pTraceShowIdForceColor' remains in code" #-}
pTraceShowIdForceColor :: (Show a) => a -> a
pTraceShowIdForceColor =
  pTraceShowIdOpt NoCheckColorTty defaultOutputOptionsDarkBg
-- | Similar to 'pTraceM', but forcing color.
{-# WARNING pTraceMForceColor "'pTraceMForceColor' remains in code" #-}
#if __GLASGOW_HASKELL__ < 800
pTraceMForceColor :: (Monad f) => String -> f ()
#else
pTraceMForceColor :: (Applicative f) => String -> f ()
#endif
pTraceMForceColor = pTraceOptM NoCheckColorTty defaultOutputOptionsDarkBg
-- | Similar to 'pTraceShowM', but forcing color.
{-# WARNING pTraceShowMForceColor "'pTraceShowMForceColor' remains in code" #-}
#if __GLASGOW_HASKELL__ < 800
pTraceShowMForceColor :: (Show a, Monad f) => a -> f ()
#else
pTraceShowMForceColor :: (Show a, Applicative f) => a -> f ()
#endif
pTraceShowMForceColor =
  pTraceShowOptM NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pTraceStack', but forcing color.
{-# WARNING pTraceStackForceColor "'pTraceStackForceColor' remains in code" #-}
pTraceStackForceColor :: String -> a -> a
pTraceStackForceColor =
  pTraceStackOpt NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pTraceEvent', but forcing color.
{-# WARNING pTraceEventForceColor "'pTraceEventForceColor' remains in code" #-}
pTraceEventForceColor :: String -> a -> a
pTraceEventForceColor =
  pTraceEventOpt NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pTraceEventIO', but forcing color.
{-# WARNING pTraceEventIOForceColor "'pTraceEventIOForceColor' remains in code" #-}
pTraceEventIOForceColor :: String -> IO ()
pTraceEventIOForceColor =
  pTraceEventOptIO NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pTraceMarker', but forcing color.
{-# WARNING pTraceMarkerForceColor "'pTraceMarkerForceColor' remains in code" #-}
pTraceMarkerForceColor :: String -> a -> a
pTraceMarkerForceColor =
  pTraceMarkerOpt NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pTraceMarkerIO', but forcing color.
{-# WARNING pTraceMarkerIOForceColor "'pTraceMarkerIOForceColor' remains in code" #-}
pTraceMarkerIOForceColor :: String -> IO ()
pTraceMarkerIOForceColor =
  pTraceMarkerOptIO NoCheckColorTty defaultOutputOptionsDarkBg

-- | Similar to 'pTraceIO', but forcing color.
{-# WARNING pTraceIOForceColor "'pTraceIOForceColor' remains in code" #-}
pTraceIOForceColor :: String -> IO ()
pTraceIOForceColor = pTraceOptIO NoCheckColorTty defaultOutputOptionsDarkBg

------------------------------------------
-- Traces without color
------------------------------------------
-- | Similar to 'pTrace', but without color.
--
-- >>> pTraceNoColor "wow" ()
-- wow
-- ()
--
-- @since 2.0.2.0
{-# WARNING pTraceNoColor "'pTraceNoColor' remains in code" #-}
pTraceNoColor :: String -> a -> a
pTraceNoColor = pTraceOpt NoCheckColorTty defaultOutputOptionsNoColor

-- | Similar to 'pTraceId', but without color.
--
-- >>> pTraceIdNoColor "(1, 2, 3)" `seq` ()
-- ( 1
-- , 2
-- , 3
-- )
-- ()
--
-- @since 2.0.2.0
{-# WARNING pTraceIdNoColor "'pTraceIdNoColor' remains in code" #-}
pTraceIdNoColor :: String -> String
pTraceIdNoColor = pTraceIdOpt NoCheckColorTty defaultOutputOptionsNoColor

-- | Similar to 'pTraceShow', but without color.
--
-- >>> import qualified Data.Map as M
-- >>> pTraceShowNoColor (M.fromList [(1, True)]) ()
-- fromList
--     [
--         ( 1
--         , True
--         )
--     ]
-- ()
--
-- @since 2.0.2.0
{-# WARNING pTraceShowNoColor "'pTraceShowNoColor' remains in code" #-}
pTraceShowNoColor :: (Show a) => a -> b -> b
pTraceShowNoColor = pTraceShowOpt NoCheckColorTty defaultOutputOptionsNoColor

-- | Similar to 'pTraceShowId', but without color.
--
-- >>> import qualified Data.Map as M
-- >>> pTraceShowIdNoColor (M.fromList [(1, True)]) `seq` ()
-- fromList
--     [
--         ( 1
--         , True
--         )
--     ]
-- ()
--
-- @since 2.0.2.0
{-# WARNING pTraceShowIdNoColor "'pTraceShowIdNoColor' remains in code" #-}
pTraceShowIdNoColor :: (Show a) => a -> a
pTraceShowIdNoColor =
  pTraceShowIdOpt NoCheckColorTty defaultOutputOptionsNoColor
-- | Similar to 'pTraceM', but without color.
--
-- >>> pTraceMNoColor "wow"
-- wow
--
-- @since 2.0.2.0
{-# WARNING pTraceMNoColor "'pTraceMNoColor' remains in code" #-}
#if __GLASGOW_HASKELL__ < 800
pTraceMNoColor :: (Monad f) => String -> f ()
#else
pTraceMNoColor :: (Applicative f) => String -> f ()
#endif
pTraceMNoColor = pTraceOptM NoCheckColorTty defaultOutputOptionsNoColor
-- | Similar to 'pTraceShowM', but without color.
--
-- >>> pTraceShowMNoColor [1,2,3]
-- [ 1
-- , 2
-- , 3
-- ]
--
-- @since 2.0.2.0
{-# WARNING pTraceShowMNoColor "'pTraceShowMNoColor' remains in code" #-}
#if __GLASGOW_HASKELL__ < 800
pTraceShowMNoColor :: (Show a, Monad f) => a -> f ()
#else
pTraceShowMNoColor :: (Show a, Applicative f) => a -> f ()
#endif
pTraceShowMNoColor = pTraceShowOptM NoCheckColorTty defaultOutputOptionsNoColor

-- | Similar to 'pTraceStack', but without color.
--
-- >>> pTraceStackNoColor "wow" () `seq` ()
-- wow
-- ()
--
-- @since 2.0.2.0
{-# WARNING pTraceStackNoColor "'pTraceStackNoColor' remains in code" #-}
pTraceStackNoColor :: String -> a -> a
pTraceStackNoColor = pTraceStackOpt NoCheckColorTty defaultOutputOptionsNoColor

-- | Similar to 'pTraceEvent', but without color.
--
-- @since 2.0.2.0
{-# WARNING pTraceEventNoColor "'pTraceEventNoColor' remains in code" #-}
pTraceEventNoColor :: String -> a -> a
pTraceEventNoColor = pTraceEventOpt NoCheckColorTty defaultOutputOptionsNoColor

-- | Similar to 'pTraceEventIO', but without color.
--
-- @since 2.0.2.0
{-# WARNING pTraceEventIONoColor "'pTraceEventIONoColor' remains in code" #-}
pTraceEventIONoColor :: String -> IO ()
pTraceEventIONoColor =
  pTraceEventOptIO NoCheckColorTty defaultOutputOptionsNoColor

-- | Similar to 'pTraceMarker', but without color.
--
-- @since 2.0.2.0
{-# WARNING pTraceMarkerNoColor "'pTraceMarkerNoColor' remains in code" #-}
pTraceMarkerNoColor :: String -> a -> a
pTraceMarkerNoColor =
  pTraceMarkerOpt NoCheckColorTty defaultOutputOptionsNoColor

-- | Similar to 'pTraceMarkerIO', but without color.
--
-- @since 2.0.2.0
{-# WARNING pTraceMarkerIONoColor "'pTraceMarkerIONoColor' remains in code" #-}
pTraceMarkerIONoColor :: String -> IO ()
pTraceMarkerIONoColor =
  pTraceMarkerOptIO NoCheckColorTty defaultOutputOptionsNoColor

-- | Similar to 'pTraceIO', but without color.
--
-- >>> pTraceIONoColor "(1, 2, 3)"
-- ( 1
-- , 2
-- , 3
-- )
--
-- @since 2.0.2.0
{-# WARNING pTraceIONoColor "'pTraceIONoColor' remains in code" #-}
pTraceIONoColor :: String -> IO ()
pTraceIONoColor = pTraceOptIO NoCheckColorTty defaultOutputOptionsNoColor

------------------------------------------
-- Traces that take options
------------------------------------------
{-|
Like 'pTrace' but takes OutputOptions.
-}
{-# WARNING pTraceOpt "'pTraceOpt' remains in code" #-}
pTraceOpt :: CheckColorTty -> OutputOptions -> String -> a -> a
pTraceOpt checkColorTty outputOptions =
  trace . unpack . pStringTTYOpt checkColorTty outputOptions

{-|
Like 'pTraceId' but takes OutputOptions.
-}
{-# WARNING pTraceIdOpt "'pTraceIdOpt' remains in code" #-}
pTraceIdOpt :: CheckColorTty -> OutputOptions -> String -> String
pTraceIdOpt checkColorTty outputOptions a =
  pTraceOpt checkColorTty outputOptions a a

{-|
Like 'pTraceShow' but takes OutputOptions.
-}
{-# WARNING pTraceShowOpt "'pTraceShowOpt' remains in code" #-}
pTraceShowOpt :: (Show a) => CheckColorTty -> OutputOptions -> a -> b -> b
pTraceShowOpt checkColorTty outputOptions =
  trace . unpack . pShowTTYOpt checkColorTty outputOptions

{-|
Like 'pTraceShowId' but takes OutputOptions.
-}
{-# WARNING pTraceShowIdOpt "'pTraceShowIdOpt' remains in code" #-}
pTraceShowIdOpt :: (Show a) => CheckColorTty -> OutputOptions -> a -> a
pTraceShowIdOpt checkColorTty outputOptions a =
  trace (unpack $ pShowTTYOpt checkColorTty outputOptions a) a

{-|
Like 'pTraceIO' but takes OutputOptions.
-}
{-# WARNING pTraceOptIO "'pTraceOptIO' remains in code" #-}
pTraceOptIO :: CheckColorTty -> OutputOptions -> String -> IO ()
pTraceOptIO checkColorTty outputOptions =
  traceIO . unpack <=< pStringTTYOptIO checkColorTty outputOptions
{-|
Like 'pTraceM' but takes OutputOptions.
-}
{-# WARNING pTraceOptM "'pTraceOptM' remains in code" #-}
#if __GLASGOW_HASKELL__ < 800
pTraceOptM :: (Monad f) => CheckColorTty -> OutputOptions -> String -> f ()
#else
pTraceOptM ::
     (Applicative f) => CheckColorTty -> OutputOptions -> String -> f ()
#endif
pTraceOptM checkColorTty outputOptions string =
  trace (unpack $ pStringTTYOpt checkColorTty outputOptions string) $ pure ()
{-|
Like 'pTraceShowM' but takes OutputOptions.
-}
{-# WARNING pTraceShowOptM "'pTraceShowOptM' remains in code" #-}
#if __GLASGOW_HASKELL__ < 800
pTraceShowOptM ::
     (Show a, Monad f) => CheckColorTty -> OutputOptions -> a -> f ()
#else
pTraceShowOptM ::
     (Show a, Applicative f) => CheckColorTty -> OutputOptions -> a -> f ()
#endif
pTraceShowOptM checkColorTty outputOptions =
  traceM . unpack . pShowTTYOpt checkColorTty outputOptions

{-|
Like 'pTraceStack' but takes OutputOptions.
-}
{-# WARNING pTraceStackOpt "'pTraceStackOpt' remains in code" #-}
pTraceStackOpt :: CheckColorTty -> OutputOptions -> String -> a -> a
pTraceStackOpt checkColorTty outputOptions =
  traceStack . unpack . pStringTTYOpt checkColorTty outputOptions

{-|
Like 'pTraceEvent' but takes OutputOptions.
-}
{-# WARNING pTraceEventOpt "'pTraceEventOpt' remains in code" #-}
pTraceEventOpt :: CheckColorTty -> OutputOptions -> String -> a -> a
pTraceEventOpt checkColorTty outputOptions =
  traceEvent . unpack . pStringTTYOpt checkColorTty outputOptions

{-|
Like 'pTraceEventIO' but takes OutputOptions.
-}
{-# WARNING pTraceEventOptIO "'pTraceEventOptIO' remains in code" #-}
pTraceEventOptIO :: CheckColorTty -> OutputOptions -> String -> IO ()
pTraceEventOptIO checkColorTty outputOptions =
  traceEventIO . unpack <=< pStringTTYOptIO checkColorTty outputOptions

{-|
Like 'pTraceMarker' but takes OutputOptions.
-}
{-# WARNING pTraceMarkerOpt "'pTraceMarkerOpt' remains in code" #-}
pTraceMarkerOpt :: CheckColorTty -> OutputOptions -> String -> a -> a
pTraceMarkerOpt checkColorTty outputOptions =
  traceMarker . unpack . pStringTTYOpt checkColorTty outputOptions

{-|
Like 'pTraceMarkerIO' but takes OutputOptions.
-}
{-# WARNING pTraceMarkerOptIO "'pTraceMarkerOptIO' remains in code" #-}
pTraceMarkerOptIO :: CheckColorTty -> OutputOptions -> String -> IO ()
pTraceMarkerOptIO checkColorTty outputOptions =
  traceMarkerIO . unpack <=< pStringTTYOptIO checkColorTty outputOptions
