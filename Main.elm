module Main exposing (..)

import Navigation
import Pages
import Home
import Html.App as App
import Html exposing (Html, Attribute, div, text)


main : Program Never
main =
    Navigation.program Pages.urlParser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }


type alias Model =
    { page : Pages.Page
    , homeModel : Home.Model
    }


initialModel : Model
initialModel =
    { page = Pages.Home
    , homeModel = Home.init
    }


init : Result String Pages.Page -> ( Model, Cmd Msg )
init result =
    urlUpdate result initialModel


type Msg
    = NoOp
    | HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeMsg m ->
            let
                ( subMdl, subCmd ) =
                    Home.update m model.homeModel
            in
                { model | homeModel = subMdl }
                    ! [ Cmd.map HomeMsg subCmd ]

        _ ->
            model ! []


urlUpdate : Result String Pages.Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Err _ ->
            model ! [ Pages.modify model.page ]

        Ok page ->
            case page of
                Pages.Home ->
                    { model
                        | page = page
                    }
                        ! [ Cmd.map HomeMsg Home.mountCmd ]


view : Model -> Html Msg
view model =
    div []
        [ viewPage model ]

viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        Pages.Home ->
            App.map HomeMsg <| Home.view model.homeModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
