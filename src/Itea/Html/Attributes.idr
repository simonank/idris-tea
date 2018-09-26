module Itea.Html.Attributes

%default total
%access public export

||| Element attirbutes
Attribute : Type
Attribute = Pair String String

||| Image dimension
data Dimension : Type where
  ||| Size in pixels
  Pixel   : Int -> Dimension
  ||| Size in percent
  Percent : Int -> Dimension

Show Dimension where
  show (Pixel x) = show x ++ "px"
  show (Percent x) = show x ++ "%"

cssClass : List String -> Attribute
cssClass = MkPair "class" . unwords

namespace Simple

  cssClass : String -> Attribute
  cssClass = cssClass . (::[])

href : String -> Attribute
href = MkPair "href"

id : String -> Attribute
id = MkPair "id"

title : String -> Attribute
title = MkPair "title"

rel : String -> Attribute
rel = MkPair "rel"

width : Dimension -> Attribute
width = MkPair "width" . show

height : Dimension -> Attribute
height = MkPair "height" . show
