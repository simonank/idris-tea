-- Module      : Itea.Html
-- Description :
-- Copyright   : (c) Simon Nielsen Knights 2018
-- License     : MIT
-- Maintainer  : simonank@mail.ru
-- Stability   : unstable
-- Portability : portable

module Itea.Html
import Itea.Html.Attributes
import Itea.Html.Events

import Control.Monad.Reader
import Control.Monad.Identity

%default total
%access public export

||| HTML element type representing the structure of the DOM tree
data HTML : Type -> Type where
  ||| Tag element
  ||| @tagname node tag
  ||| @events list of events assigned to the tag
  ||| @attributes tag attributes such as class, href, rel
  ||| @html child nodes in the DOM tree
  HtmlElem : (tagname    : String)
          -> (events     : List (Event a))
          -> (attributes : List Attribute)
          -> (html       : List (HTML a))
          -> HTML a
  ||| Text element
  ||| @text raw text
  HtmlText : (text : String) -> HTML a

||| HTML monad for implictly passing the event queue around
Html : Type -> Type
Html a = Reader (EventQueue a) (HTML a)

staticElement : String -> List Attribute -> List (Html a) -> Html a
staticElement name attr html =
  let html = map (runIdentity . flip runReaderT !ask) html in
  pure $ HtmlElem name [] attr html

interactiveElement : String
                  -> (xs : List (EventQueue a -> Event a))
                  -> List Attribute
                  -> { auto ok : NonEmpty xs }
                  -> Html a
interactiveElement name events attr =
  pure $ HtmlElem name (map (flip apply !ask) events) attr []

||| Construct a text element
text : String -> Html a
text = pure . HtmlText

div : List Attribute -> List (Html a) -> Html a
div = staticElement "div"

body : List Attribute -> List (Html a) -> Html a
body = staticElement "body"

nav : List Attribute -> List (Html a) -> Html a
nav = staticElement "nav"

article : List Attribute -> List (Html a) -> Html a
article = staticElement "article"

aside : List Attribute -> List (Html a) -> Html a
aside = staticElement "aside"

h1 : List Attribute -> List (Html a) -> Html a
h1 = staticElement "h1"

h2 : List Attribute -> List (Html a) -> Html a
h2 = staticElement "h2"

h3 : List Attribute -> List (Html a) -> Html a
h3 = staticElement "h3"

h4 : List Attribute -> List (Html a) -> Html a
h4 = staticElement "h4"

h5 : List Attribute -> List (Html a) -> Html a
h5 = staticElement "h5"

h6 : List Attribute -> List (Html a) -> Html a
h6 = staticElement "h6"

br : Html a
br = staticElement "br" [] []

tr : List Attribute -> List (Html a) -> Html a
tr = staticElement "tr"

table : List Attribute -> List (Html a) -> Html a
table = staticElement "table"

td : List Attribute -> List (Html a) -> Html a
td = staticElement "td"

tbody : List Attribute -> List (Html a) -> Html a
tbody = staticElement "tbody"

thead : List Attribute -> List (Html a) -> Html a
thead = staticElement "thead"

tfoot : List Attribute -> List (Html a) -> Html a
tfoot = staticElement "tfoot"

th : List Attribute -> List (Html a) -> Html a
th = staticElement "th"

||| Image object
record Image where
  constructor MkImage
  ||| Relative link
  rel    : String
  ||| Width of image
  width  : Dimension
  ||| Height of image
  height : Dimension

img : List Attribute -> Html a
img = flip (staticElement "img") []

namespace Simple

  ||| Simple interface working with images directly
  img : List Attribute -> Image -> Html a
  img attr (MkImage rel' width' height') =
    img $ [ width width'
          , height height'
          , rel rel'
          ] ++ attr

{-
header
footer
address
main
p
hr
pre
blockquote
ol
ul
li
dl
dd
figure
figcaption
a
em
strong
small
s
cite
q
abbr
time
code
var
samp
kbd
sup
i
u
matk
ruby
rt
rp
bdi
bdo
span
ins
del
iframe
embed
object
param
video
audio
source
track
canvas
math
caption
colgroup
col
form
fieldset
legend
label
select
datalist
optgroup
option
textarea
keygen
output
progress
meter
details
summary
menuitem
menu
-}

||| Interactive button
|||
||| @xs list of listeners along with their return messages
||| @attr button attributes
||| @ok proof of the input list having at least one element
button : (xs   : List (EventQueue a -> Event a))
      -> (attr : List Attribute)
      -> { auto ok : NonEmpty xs }
      -> Html a
button = interactiveElement "button"

||| Interactive input
|||
||| @xs list of listeners along with their return messages
||| @attr input attributes
||| @ok proof of the input list having at least one element
input : (xs   : List (EventQueue a -> Event a))
     -> (attr : List Attribute)
     -> { auto ok : NonEmpty xs }
     -> Html a
input = interactiveElement "input"
