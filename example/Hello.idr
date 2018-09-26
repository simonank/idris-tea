-- Module      : Main
-- Description :
-- Copyright   : (c) Simon Nielsen Knights 2018
-- License     : MIT
-- Maintainer  : simonank@mail.ru
-- Stability   : unstable
-- Portability : portable

import Itea

hello : Html a
hello = div [] [text "Hello, world!"]

main : JS_IO ()
main = ?runProgram
