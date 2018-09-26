-- Module      : Itea.Dom.Primitives
-- Description :
-- Copyright   : (c) Simon Nielsen Knights 2018
-- License     : MIT
-- Maintainer  : simonank@mail.ru
-- Stability   : unstable
-- Portability : portable

module Itea.Dom.Primitives
import Itea.Html
import Itea.Html.Events
import Itea.Html.Attributes
import Itea.Javascript.Types
import Itea.Utils
-- This part is full of belive_me as it's required when working with opaque
-- types. Type safety is regained by the node type preventing any other place
-- than the DOM from taking a DOM pointer. The lack of constructors here is for
-- performance as wrapping JS values in a data type won't really change much
-- since they're opaque.

%default covering
%access export

||| The opaque type representing a DOM node
Node : Type
Node = JSRef 'Node

believeIO : JS_IO Ptr -> JS_IO a
believeIO = believe_me

document : JS_IO Node
document = believeIO $ jscall "document" _

documentBody : JS_IO Node
documentBody = believeIO $ jscall "document.body" _

createTextNode : String -> JS_IO Node
createTextNode = believeIO . jscall "document.createTextNode(%0)" _

createElement : String -> JS_IO Node
createElement = believeIO . jscall "document.createElement(%0)" _

setInnerHtml : Node -> String -> JS_IO ()
setInnerHtml node = jscall "%0.setInnerHTML = %1"
                           (Ptr -> String -> JS_IO ())
                           (believe_me node)

getElementById : String -> JS_IO Node
getElementById = believeIO . jscall "document.getElementById(%0)" _

nthChild : Node -> Int -> JS_IO Node
nthChild node = believeIO . jscall "%0.childNodes[%0]"
                                   (Ptr -> Int -> JS_IO Ptr)
                                   (believe_me node)

appendChild : Node -> Node -> JS_IO Node
appendChild parent child =
  believeIO $ jscall "%0.appendChild(%1)"
                     (Ptr -> Ptr -> JS_IO Ptr)
                     (believe_me parent)
                     (believe_me child)

replaceChild : Node -> Node -> Node -> JS_IO ()
replaceChild parent old new =
  jscall "%0.replaceChild(%1,%2)"
         (Ptr -> Ptr -> Ptr -> JS_IO ())
         (believe_me parent)
         (believe_me old)
         (believe_me new)

removeChild : Node -> Node -> JS_IO Node
removeChild parent child =
  believeIO $ jscall "%0.removeChild(%1)"
                     (Ptr -> Ptr -> JS_IO Ptr)
                     (believe_me parent)
                     (believe_me child)

setAttribute : Node -> String -> String -> JS_IO ()
setAttribute node = jscall "%0.setAttribute(%1,%2)"
                           (Ptr -> String -> String -> JS_IO ())
                           (believe_me node)

removeAttribute : Node -> String -> JS_IO ()
removeAttribute node =
  jscall "%0.removeAttribute(%1)"
         (Ptr -> String -> JS_IO ())
         (believe_me node)

addEventListener : Node
                -> String
                -> JsFn (Ptr -> JS_IO ())
                -> JSRef 'EventOptions
                -> JS_IO ()
addEventListener node name fn opt =
  jscall "%0.addEventListener(%1,%2,%3)"
         (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> Ptr -> JS_IO ())
         (believe_me node) name fn (believe_me opt)

removeEventListener : Node
                   -> String
                   -> JsFn (Ptr -> JS_IO ())
                   -> JSRef 'EventOptions
                   -> JS_IO ()
removeEventListener node name fn opt =
  jscall "%0.removeEventListener(%1,%2,%3)"
         (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> Ptr -> JS_IO ())
         (believe_me node) name fn (believe_me opt)

addEventHandler : Node -> Event a -> JS_IO ()
addEventHandler node (MkEvent name handler options) =
  addEventListener node name handler options

updateAttributes : Node
                -> List (String, String)
                -> List (String, String)
                -> JS_IO ()
updateAttributes node old new =
  do traverse_ (removeAttribute node . fst) old
     traverse_ (uncurry $ setAttribute node) new

createDomNode : HTML a -> JS_IO Node
createDomNode (HtmlElem tagname events attributes html) =
  do nodes <- traverse createDomNode html
     tag   <- createElement tagname
     traverse_ (addEventHandler tag) events
     updateAttributes tag [] attributes
     traverse_ (appendChild tag) nodes
     pure tag
createDomNode (HtmlText text) =
  createTextNode text

render : Node -> Maybe (HTML a) -> Maybe (HTML a) -> JS_IO ()
render root old new = renderList (indexed $ toList old) (toList new)
  -- TODO diffing
  where

    renderList : List (Int, HTML a) -> List (HTML a) -> JS_IO ()
    renderList old new = setInnerHtml root ""
                      *> traverse createDomNode new
                      >>= traverse_ (appendChild root)
