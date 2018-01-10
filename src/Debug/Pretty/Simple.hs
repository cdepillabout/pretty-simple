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
  ) where

import Data.Text.Lazy (unpack)
import Debug.Trace
       (trace, traceEvent, traceEventIO, traceIO, traceM, traceMarker,
        traceMarkerIO, traceStack)
import Text.Pretty.Simple (pShow, pShowNoColor, pString, pStringNoColor)

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
pTraceIO :: String -> IO ()
pTraceIO = traceIO . unpack . pString

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
pTrace :: String -> a -> a
pTrace = trace . unpack . pString

{-|
Like 'pTrace' but returns the message instead of a third value.

@since 2.0.1.0
-}
pTraceId :: String -> String
pTraceId a = pTrace a a

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
pTraceShow :: (Show a) => a -> b -> b
pTraceShow = trace . unpack . pShow

{-|
Like 'pTraceShow' but returns the shown value instead of a third value.

@since 2.0.1.0
-}
pTraceShowId :: (Show a) => a -> a
pTraceShowId a = trace (unpack (pShow a)) a

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
#if __GLASGOW_HASKELL__ < 800
pTraceM :: (Monad f) => String -> f ()
#else
pTraceM :: (Applicative f) => String -> f ()
#endif
pTraceM string = trace (unpack (pString string)) $ pure ()

{-|
Like 'pTraceM', but uses 'show' on the argument to convert it to a 'String'.

> ... = do
>   x <- ...
>   pTraceShowM $ x
>   y <- ...
>   pTraceShowM $ x + y

@since 2.0.1.0
-}
#if __GLASGOW_HASKELL__ < 800
pTraceShowM :: (Show a, Monad f) => a -> f ()
#else
pTraceShowM :: (Show a, Applicative f) => a -> f ()
#endif
pTraceShowM = traceM . unpack . pShow

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
pTraceStack :: String -> a -> a
pTraceStack = traceStack . unpack . pString

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
pTraceEvent :: String -> a -> a
pTraceEvent = traceEvent . unpack . pString

{-|
The 'pTraceEventIO' function emits a message to the eventlog, if eventlog
profiling is available and enabled at runtime.

Compared to 'pTraceEvent', 'pTraceEventIO' sequences the event with respect to
other IO actions.

@since 2.0.1.0
-}
pTraceEventIO :: String -> IO ()
pTraceEventIO = traceEventIO . unpack . pString

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
pTraceMarker :: String -> a -> a
pTraceMarker = traceMarker . unpack . pString

-- | The 'pTraceMarkerIO' function emits a marker to the eventlog, if eventlog
-- profiling is available and enabled at runtime.
--
-- Compared to 'pTraceMarker', 'pTraceMarkerIO' sequences the event with respect
-- to other IO actions.
--
-- @since 2.0.1.0
pTraceMarkerIO :: String -> IO ()
pTraceMarkerIO = traceMarkerIO . unpack . pString

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
pTraceNoColor :: String -> a -> a
pTraceNoColor = trace . unpack . pStringNoColor

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
pTraceIdNoColor :: String -> String
pTraceIdNoColor a = pTraceNoColor a a

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
pTraceShowNoColor :: (Show a) => a -> b -> b
pTraceShowNoColor = trace . unpack . pShowNoColor

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
pTraceShowIdNoColor :: (Show a) => a -> a
pTraceShowIdNoColor a = trace (unpack (pShowNoColor a)) a

-- | Similar to 'pTraceM', but without color.
--
-- >>> pTraceMNoColor "wow"
-- wow
--
-- @since 2.0.2.0
#if __GLASGOW_HASKELL__ < 800
pTraceMNoColor :: (Monad f) => String -> f ()
#else
pTraceMNoColor :: (Applicative f) => String -> f ()
#endif
pTraceMNoColor string = trace (unpack (pString string)) $ pure ()

-- | Similar to 'pTraceShowM', but without color.
--
-- >>> pTraceShowMNoColor [1,2,3]
-- [ 1
-- , 2
-- , 3
-- ]
--
-- @since 2.0.2.0
#if __GLASGOW_HASKELL__ < 800
pTraceShowMNoColor :: (Show a, Monad f) => a -> f ()
#else
pTraceShowMNoColor :: (Show a, Applicative f) => a -> f ()
#endif
pTraceShowMNoColor = traceM . unpack . pShowNoColor

-- | Similar to 'pTraceStack', but without color.
--
-- >>> pTraceStackNoColor "wow" () `seq` ()
-- wow
-- ()
--
-- @since 2.0.2.0
pTraceStackNoColor :: String -> a -> a
pTraceStackNoColor = traceStack . unpack . pStringNoColor

-- | Similar to 'pTraceEvent', but without color.
--
-- @since 2.0.2.0
pTraceEventNoColor :: String -> a -> a
pTraceEventNoColor = traceEvent . unpack . pStringNoColor

-- | Similar to 'pTraceEventIO', but without color.
--
-- @since 2.0.2.0
pTraceEventIONoColor :: String -> IO ()
pTraceEventIONoColor = traceEventIO . unpack . pStringNoColor

-- | Similar to 'pTraceMarker', but without color.
--
-- @since 2.0.2.0
pTraceMarkerNoColor :: String -> a -> a
pTraceMarkerNoColor = traceMarker . unpack . pStringNoColor

-- | Similar to 'pTraceMarkerIO', but without color.
--
-- @since 2.0.2.0
pTraceMarkerIONoColor :: String -> IO ()
pTraceMarkerIONoColor = traceMarkerIO . unpack . pStringNoColor

-- | Similar to 'pTraceIO', but without color.
--
-- >>> pTraceIONoColor "(1, 2, 3)"
-- ( 1
-- , 2
-- , 3
-- )
--
-- @since 2.0.2.0
pTraceIONoColor :: String -> IO ()
pTraceIONoColor = traceIO . unpack . pStringNoColor
