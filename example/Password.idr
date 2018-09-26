-- Module      : Main
-- Description :
-- Copyright   : (c) Simon Nielsen Knights 2018
-- License     : MIT
-- Maintainer  : simonank@mail.ru
-- Stability   : unstable
-- Portability : portable

import Itea

||| Message sent upon user generated event
data Msg
  = Edited

data Reason
  = TooShort
  | DoNotMatch
  | Ok

||| Application model consisting of the applicaiton state
record Model where
  constructor MkModel
  password      : String
  passwordAgain : String

||| Initial state
init : (Model, Cmd Msg)
init = (MkModel "" "", ?nothing)

||| Update state upon event
update : Msg -> Model -> (Model, Cmd Msg)
update Edited model = (model, ?cmd)

||| Subscriptions to IO replated services
subscriptions : Model -> Sub Msg
subscriptions _ = ?none

||| Display the input boxes along with the result
prompt : Reason -> String -> Html Msg
prompt state ref =
    div [cssClass "passwordbox"]
      [ text "password: "
      , input [onchange Edited] []
      , text "again: "
      , input [onchange Edited] match
      , div reason [text ref]
      ]

  where

    ||| Change password box theme to reflect a difference
    match : List Attribute
    match = case state of
                 DoNotMatch => [cssClass "nomatch"]
                 _          => []

    ||| Display the status of the entry
    reason : List Attribute
    reason = [ case state of
                    DoNotMatch => cssClass "red"
                    TooShort   => cssClass "yellow"
                    Ok         => cssClass "green"
             ]

||| View which builds the page using the model
view : Model -> Html Msg
view (MkModel pass again) =
  case (pass == again, length pass > 8) of
       (_, True)  => prompt TooShort "Too short"
       (False, _) => prompt DoNotMatch "Passwords don't match"
       (True, _)  => prompt Ok "All ok"

||| Program entry
main : JS_IO ()
main = runProgram $ record
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  } ?default
