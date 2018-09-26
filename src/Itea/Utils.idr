-- Module      : Itea.Utils
-- Description :
-- Copyright   : (c) Simon Nielsen Knights 2018
-- License     : MIT
-- Maintainer  : simonank@mail.ru
-- Stability   : unstable
-- Portability : portable

module Itea.Utils
import Language.Reflection.Utils

%language ElabReflection
%default total
%access public export

jsbool : Bool -> Int
jsbool True = 1
jsbool False = 0

||| Call a native javascript procedure
|||
||| @native Javascript procedure/expression to call
||| @type type of given procedure/expression
||| @ok proof that the given type satisfies the FFI_JS interface
jscall : (native : String)
      -> (type : Type)
      -> { auto ok : FTy FFI_JS [] type }
      -> type
jscall name type = foreign FFI_JS name type

||| Log a string to the console
consoleLog : String -> JS_IO ()
consoleLog = jscall "console.log(%0)" (String -> JS_IO ())

||| Display the alert dialog with given text
alert : String -> JS_IO ()
alert = jscall "alert(%0)" (String -> JS_IO ())

||| Print a message for debugging with source location as a side effect
|||
||| @loc call site location
||| @message message to display
||| @result the result returned
trace : {default (%runElab sourceLocation) loc : SourceLocation}
     -> (message : String)
     -> (result : a)
     -> a
trace {loc} text res = unsafePerformIO {ffi=FFI_JS}
  (consoleLog ("[T] " ++ show loc ++ "::" ++ text) *> pure res)
-- %deprecated trace "Debug function `trace' used in source file"

||| Index a list given a constructor/function and a stream of values
indexdWith : (f : (a -> b -> c)) -> Stream a -> List b -> List c
indexdWith _ _ [] = []
indexdWith f (x :: xs) (y :: ys) = f x y :: indexdWith f xs ys

||| Index a list elements with their position
indexed : (Enum b, Num b) => List a -> List (b, a)
indexed = indexdWith MkPair [0..]

