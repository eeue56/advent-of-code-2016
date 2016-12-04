port module Main exposing (..)


import Html exposing (text, div, textarea, input, Html)
import Html.Attributes exposing (defaultValue, style)
import Html.Events exposing (..)


type alias Model =
    { input : String
    , response : String
    }

init : (Model, Cmd Msg)
init = ( Model "input" "output", Cmd.none )

type Msg
    = SendTextToElm String
    | ReceiveTextFromElm
    | UpdateInput String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendTextToElm str ->
            ({ model | response = str }, Cmd.none)

        ReceiveTextFromElm ->
            (model, receiveTextFromElm model.input)

        UpdateInput str ->
            ({ model | input = str }, receiveTextFromElm str)


props : Html.Attribute msg
props =
    style [ ("width", "200px"), ("height", "300px") ]


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.textarea [ onInput UpdateInput, props, defaultValue model.input ] [  ]
        , Html.textarea [ props, defaultValue model.response ] []
        , Html.button [ onClick ReceiveTextFromElm ] [ text "Type (or click) to send text to Idris!"]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    sendTextToElm SendTextToElm

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


-- these names are bad so they make more sense in idris land

-- when we send things from idris to elm
port sendTextToElm : (String -> msg) -> Sub msg

-- when we receive things from elm
port receiveTextFromElm : String -> Cmd msg
