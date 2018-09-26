module Itea.Program
import Itea.Html
import Itea.Dom.Primitives

||| Commands
data Cmd : Type -> Type where

||| Subscriptions
data Sub : Type -> Type where

||| Data representation of the program which carries all information
||| to run the applicationg indefinetly
|||
||| @model the used supplied model (state) for the program
||| @msg the message type representing user actions
record Program model msg where
  constructor MkProgram
  init          : (model, Cmd msg)
  update        : msg -> model -> (model, Cmd msg)
  subscriptions : model -> Sub msg
  view          : model -> Html msg



runProgram : Program model msg -> JS_IO ()
runProgram (MkProgram init update subscriptions view) = loop init

  where

    -- The main event loop handles fetching events from the event
    -- queue, values from subscriptions. This provides the interface
    -- where we focus on events over time rather than having to deal
    -- explicitly with the looping details ourselves.
    loop : (model, Cmd msg) -> JS_IO ()
    loop =
      do ?k
         -- setTimeout (loop ?p) 50
         -- pure ()


