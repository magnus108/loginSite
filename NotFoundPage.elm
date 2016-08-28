module NotFoundPage exposing (..)

import Pages
import Html exposing (Html, Attribute, div, text, h3, p)
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
    { title = "Not Found"
    , text = "The Page you are looking for was not found"
    , linkText = "frontpage"
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
                [ p [] [ text model.text ]
                , p [] [ Pages.linkTo (Pages.LoginPage) [class [MainCss.Link]] [ text model.linkText ]]
                ]
    in
        div [] [ head, body ]
