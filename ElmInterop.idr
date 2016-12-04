module ElmInterop

import IdrisScript

%access public export

ElmApp : Type
ElmApp = Ptr

startElm : () -> JS_IO ElmApp
startElm x = do
    jscall """
(function() {
    console.log('here..');
    var elm = Elm.Main.fullscreen();
    return elm;
})()
"""
        (() -> JS_IO ElmApp)
        x

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


makeAdventApp : (String -> String) -> JS_IO ()
makeAdventApp converter = do
    app <- startElm ()
    receiveTextFromElm (sendTextToElm app . converter) app
    pure ()
