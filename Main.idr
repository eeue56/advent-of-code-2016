module Main

import ElmInterop
import DayTwo

-- a simple function just to say hi to elm
helloFromIdris : String -> String
helloFromIdris text =
    "hello from idris! You sent me '" ++ text ++ "' from Elm!"

-- a plain simple example hello world app
helloMain : JS_IO ()
helloMain =
    makeAdventApp helloFromIdris

-- the day two problem example
dayTwoMain : JS_IO ()
dayTwoMain =
    makeAdventApp
        (\str =>
            show $ collectGridPoints defaultKeypad (movesFromLines str) defaultPosition
        )

-- change this to whatever day you want to work on
main : JS_IO ()
main = do
    dayTwoMain