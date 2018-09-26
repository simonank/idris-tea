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

setInterval : (() -> JS_IO ()) -> Int -> JS_IO Interval
setInterval fn time =
  MkInterval <$> jscall "setInterval(%0,%1)"
                        (JsFn (() -> JS_IO ()) -> Int -> JS_IO Ptr)
                        (MkJsFn fn) time

clearInterval : Interval -> JS_IO ()
clearInterval = jscall "clearInterval(%0)" (Ptr  -> JS_IO ()) . unInterval

setTimeout : (() -> JS_IO ()) -> Int -> JS_IO Timeout
setTimeout fn time =
  MkTimeout <$> jscall "setTimeout(%0,%1)"
                      (JsFn (() -> JS_IO ()) -> Int -> JS_IO Ptr)
                      (MkJsFn fn) time

clearTimeout : Timeout -> JS_IO ()
clearTimeout = jscall "clearTimeout(%0)" (Ptr  -> JS_IO ()) . unTimeout


