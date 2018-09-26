module Itea.Html.Events
import Itea.Utils
import Itea.Javascript.Types

import Control.Monad.Reader
import Control.Monad.Trans
import Data.IORef

%default total
%access public export

||| Internal event queue used for feeding user actions and other events for
||| the main loop to consume.
data EventQueue : Type -> Type where
  MkEVQ : IORef (List a) -> EventQueue a

||| Event handler type
Handler : Type -> Type
Handler a = JsFn (Ptr -> JS_IO ())

||| Event type
data Event : Type -> Type where
  MkEvent : String -> Handler a -> JSRef 'EventOptions -> Event a

HtmlEvent : Type -> Type -> Type
HtmlEvent a b = ReaderT (EventQueue a, Ptr) JS_IO b

||| Place a new event on the event queue
putEvent : EventQueue a -> a -> JS_IO ()
putEvent (MkEVQ queue) value = modifyIORef' queue (value::)

||| Pop the latest event from the event queue
getEvent : EventQueue a -> JS_IO (Maybe a)
getEvent (MkEVQ queue) =
  case !(readIORef' queue) of
       (x :: _) => modifyIORef' queue (drop 1) *> pure (Just x)
       _        => pure Nothing

namespace HtmlEvent

  putEvent : a -> HtmlEvent a ()
  putEvent value = asks Prelude.Basics.fst
               >>= lift . flip putEvent value

  getEvent : HtmlEvent a (Maybe a)
  getEvent = asks Prelude.Basics.fst
         >>= lift . getEvent

event : String -> JSRef 'EventOptions -> HtmlEvent a () -> EventQueue a -> Event a
event name options handler e =
  MkEvent name
          (MkJsFn $ \p => runReaderT handler (e,p))
          options

onchange : JSRef 'EventOptions
        -> HtmlEvent a ()
        -> EventQueue a
        -> Event a
onchange = event "change"

onclick : JSRef 'EventOptions
       -> HtmlEvent a ()
       -> EventQueue a
       -> Event a
onclick = event "click"

options : (passive : Bool) -> (once : Bool) -> (capture : Bool) -> JSRef 'EventOptions
options p o c = believe_me
              $ jscall "{'passive': %0, 'once': %1, 'capture': %2}"
                       (Int -> Int -> Int -> JS_IO Ptr)
                       (jsbool p) (jsbool o) (jsbool c)

namespace Default

  ||| Default options for onchange
  onchange : HtmlEvent a () -> EventQueue a -> Event a
  onchange = onchange (options False False False)

  ||| Default options for onclick
  onclick : HtmlEvent a () -> EventQueue a -> Event a
  onclick = onclick (options False False False)

namespace Simple

  onchange : a -> EventQueue a -> Event a
  onchange = onchange . putEvent

  onclick : a -> EventQueue a -> Event a
  onclick = onclick . putEvent


