module Itea.Javascript.Objects
import Itea.Javascript.Types
import Itea.Utils

import Data.List

%default total
%access public export

||| Interface for inheriting native Javascript object properties
||| of common objects
interface Inherit obj where
  property   : List String
  prototype : List String

||| Opaque type representing a javascript object
data Object : List String -> Type where

||| Extend a given object with a property if such
||| property is not present.
|||
||| @prop property to extend object with
||| @value object to set property to
||| @object object to extend
||| @ok proof of property not existing
extendProperty : Convertible a =>
         (prop   : String)
      -> (value  : a)
      -> (object : Object xs)
      -> { auto ok : Not (Elem prop xs) }
      -> Object (prop :: xs)
extendProperty prop value object = believe_me $
  foreign FFI_JS ("%0." ++ prop ++ " = %1")
                 (Ptr -> Ptr -> JS_IO ())
                 (believe_me object) (convert value)

||| Revoke an existing property from
||| an object
|||
||| @prop property to revoke
||| @object object to revoke property from
||| @ok proof of property existing
revokeProperty : (prop   : String)
              -> (object : Object xs)
              -> { auto ok : Elem prop xs }
              -> Object (filter (/=prop) xs)
revokeProperty prop object = believe_me $ object
