-- Module      : Itea.Javascript.Timer
-- Description :
-- Copyright   : (c) Simon Nielsen Knights 2018
-- License     : MIT
-- Maintainer  : simonank@mail.ru
-- Stability   : unstable
-- Portability : portable

module Itea.Javascript.Timer
import Itea.Utils

%default covering
%access public export

record Interval where
  constructor MkInterval
  unInterval : Ptr

record Timeout where
  constructor MkTimeout
  unTimeout : Ptr

||| Set an interval a command should be run on
|||
||| @fn command to run
||| @interval interval in milliseconds
setInterval : (fn : (() -> JS_IO ())) -> (interval : Int) -> JS_IO Interval
setInterval fn time =
  MkInterval <$> jscall "setInterval(%0,%1)"
                        (JsFn (() -> JS_IO ()) -> Int -> JS_IO Ptr)
                        (MkJsFn fn) time

||| Clear the interval; this stops future execution of the given command
clearInterval : Interval -> JS_IO ()
clearInterval = jscall "clearInterval(%0)" (Ptr  -> JS_IO ()) . unInterval

||| Run function after a specific time has passed
|||
||| @fn command to run
||| @timeout time to delay before running the command
setTimeout : (fn : (() -> JS_IO ())) -> (timeout : Int) -> JS_IO Timeout
setTimeout fn time =
  MkTimeout <$> jscall "setTimeout(%0,%1)"
                      (JsFn (() -> JS_IO ()) -> Int -> JS_IO Ptr)
                      (MkJsFn fn) time

||| Clear timeout; this stops further execution of the given command
clearTimeout : Timeout -> JS_IO ()
clearTimeout = jscall "clearTimeout(%0)" (Ptr  -> JS_IO ()) . unTimeout


