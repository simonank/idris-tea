{-
Copyright 2018 Simon Nielsen Knights

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

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
