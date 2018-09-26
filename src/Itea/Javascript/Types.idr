-- Module      : Itea.Javascript.Types
-- Description :
-- Copyright   : (c) Simon Nielsen Knights 2018
-- License     : MIT
-- Maintainer  : simonank@mail.ru
-- Stability   : unstable
-- Portability : portable

module Itea.Javascript.Types
import Itea.Utils

%default total
%access public export

||| Opaque type for javascript references
data JSRef : Type -> Type where

||| Conversion from Idris values to Javascript values
interface Convertible a where
  convert : a -> Ptr
-- TODO Write implementations for more interesting structures

Convertible (JSRef a) where
  convert = believe_me

data JSType
  = JSNumber
  | JSString
  | JSBoolean
  | JSFunction
  | JSUndefined
  | JSObject String
  | JSNull

||| Return the constructor name
export
constructorName : JSRef a -> JS_IO String
constructorName = jscall "%0.constructor.name" (Ptr -> JS_IO String) . convert

||| Return the Javascript type of the given object
export
typeOf : JSRef a -> JS_IO JSType
typeOf p = case !(jscall check (Ptr -> JS_IO Int) (convert p)) of
                0 => pure JSNumber
                1 => pure JSString
                2 => pure JSBoolean
                3 => pure JSFunction
                4 => pure JSUndefined
                5 => pure $ JSObject !(constructorName p)
                _ => pure JSNull

  where

    check : String
    check ="""
(function (p) {
  if (typeof p == 'number') return 0;
  if (typeof p == 'string') return 1;
  if (typeof p == 'boolean') return 2;
  if (typeof p == 'function') return 3;
  if (typeof p == 'undefined') return 4;
  if (typeof p == 'object') return 5;
  return 6;})(%0)"""
