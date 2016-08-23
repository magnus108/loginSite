module NotFoundPage exposing (..)

import Pages
import Html exposing (Html, Attribute, div, text, form, input, h3, ul, li)
import Html.Attributes exposing (type', placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task


type alias Model =
    { title : String
    , text : String
    }


emptyModel : Model
emptyModel =
    { title = "Not Found"
    , text = "The Page you are looking for was not found"
    }


init : Model
init =
    emptyModel


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []


view : Model -> Html Msg
view model =
    let
        head =
            h3 [] [ text model.title ]

        body =
            div []
                [ text model.text
                ]
    in
        div [] [ head, body ]
