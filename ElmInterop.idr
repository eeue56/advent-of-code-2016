module ElmInterop

import IdrisScript
import Debug.Trace as Debug

%access public export

ElmApp : Type
ElmApp = Ptr

startElm : () -> JS_IO ElmApp
startElm x = do
    jscall """
(function() {
    console.log('here..');
    var elm = Elm.Main.fullscreen({});
    return elm;
})()
"""
        (() -> JS_IO ElmApp)
        x

loadFromLocalStorage : String -> JS_IO (Maybe String)
loadFromLocalStorage key = do
    value <- jscall """
(function(key) {
    var item = localStorage.getItem(key);
    if (item === null) { return "" };
    return item;
})(%0)
"""
        (String -> JS_IO (String))
        key

    case value of
        "" => pure Nothing
        v => pure $ Just v

storeInLocalStorage : String -> String -> JS_IO ()
storeInLocalStorage key value = do
    jscall """
(function(key, value) {
    localStorage[key] = value;
})(%0, %1)
"""
        (String -> String -> JS_IO ())
        key value

sendTextToElm : ElmApp -> String -> JS_IO ()
sendTextToElm app text = do
    jscall """
(function(elm, text) {
    elm.ports.sendTextToElm.send(text);
})(%0, %1)
"""
        (ElmApp -> String -> JS_IO ())
        app text

receiveTextFromElm : (String -> JS_IO ()) -> ElmApp -> JS_IO ()
receiveTextFromElm fn app =
    jscall """
(function(elm, fn) {
    elm.ports.receiveTextFromElm.subscribe(fn);
})(%0, %1)
"""
        (ElmApp -> JsFn (String -> JS_IO ()) -> JS_IO ())
        app (MkJsFn fn)

sendInputToElm : ElmApp -> String -> JS_IO ()
sendInputToElm app text = do
    jscall """
(function(elm, text) {
    elm.ports.sendInputToElm.send(text);
})(%0, %1)
"""
        (ElmApp -> String -> JS_IO ())
        app text

makeAdventApp : (String -> String) -> JS_IO ()
makeAdventApp converter = do
    app <- startElm ()
    receiveTextFromElm (sendTextToElm app . converter) app

    storedThing <- loadFromLocalStorage "advent-input"

    case storedThing of
        Nothing => pure ()
        Just value => sendInputToElm app value

    receiveTextFromElm (storeInLocalStorage "advent-input") app
    pure ()
