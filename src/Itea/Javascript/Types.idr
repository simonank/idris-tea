module Itea.Javascript.Types
import Itea.Utils

%default total
%access public export

||| Opaque type for javascript references
data JSRef : Type -> Type where

||| Conversion from Idris values to Javascript values
interface Convertible a where
  convert : a -> Ptr

Convertible Int where
  convert = believe_me

Convertible String where
  convert = believe_me

Convertible Double where
  convert = believe_me

Convertible Bool where
  convert = believe_me . jsbool

Convertible (JSRef a) where
  convert = believe_me


