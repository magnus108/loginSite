module LogoutPage exposing (..)

import Pages
import Html exposing (Html, Attribute, div, text, form, input, h3, ul, li, p)
import Html.Attributes exposing (type', placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task

import MainCss

type alias Model =
    { title : String
    , text : String
    , linkText : String
    }


emptyModel : Model
emptyModel =
    { title = "You are logged out"
    , text = "click here to Login"
    , linkText = "Login"
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
        { class } =
            MainCss.navbarNamespace

        head =
            h3 [] [ text model.title ]

        body =
            div []
                [ p [] [text model.text]
                , p [] [Pages.linkTo (Pages.LoginPage) [class [MainCss.Link]] [ text model.linkText ]]
                ]
    in
        div [] [ head, body ]
