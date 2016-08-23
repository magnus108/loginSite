module LogoutPage exposing (..)

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
    , linkText : String
    }


emptyModel : Model
emptyModel =
    { title = "You are logged out"
    , text = "click here to Login"
    , linkText = "Go to Login"
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
                , Pages.linkTo (Pages.LoginPage) [] [ text model.linkText ]
                ]
    in
        div [] [ head, body ]
