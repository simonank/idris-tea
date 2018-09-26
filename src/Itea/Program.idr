-- Module      : Itea.Program
-- Description :
-- Copyright   : (c) Simon Nielsen Knights 2018
-- License     : MIT
-- Maintainer  : simonank@mail.ru
-- Stability   : unstable
-- Portability : portable

module Itea.Program
import Itea.Html
import Itea.Dom.Primitives

%default total
%access public export

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
  ||| Initial state
  init          : (model, Cmd msg)
  ||| Update function used to advance state by one step
  update        : msg -> model -> (model, Cmd msg)
  ||| Subscription to services which fire events upon completion
  subscriptions : model -> Sub msg
  ||| View constructing the DOM tree
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


