module Main exposing (..)

import Navigation
import Pages
import Home
import Travel
import UserPage
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
    , travelModel : Travel.Model
    , userPageModel : UserPage.Model
    }


initialModel : Model
initialModel =
    { page = Pages.Home
    , homeModel = Home.init
    , travelModel = Travel.init
    , userPageModel = UserPage.init
    }


init : Result String Pages.Page -> ( Model, Cmd Msg )
init result =
    urlUpdate result initialModel


type Msg
    = NoOp
    | HomeMsg Home.Msg
    | TravelMsg Travel.Msg
    | UserPageMsg UserPage.Msg


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

        TravelMsg m ->
            let
                ( subMdl, subCmd ) =
                    Travel.update m model.travelModel
            in
                { model | travelModel = subMdl }
                    ! [ Cmd.map TravelMsg subCmd ]

        UserPageMsg m ->
            let
                ( subMdl, subCmd ) =
                    UserPage.update m model.userPageModel
            in
                { model | userPageModel = subMdl }
                    ! [ Cmd.map UserPageMsg subCmd ]

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

                Pages.TravelPage id ->
                    { model
                        | page = page
                    }
                        ! [ Cmd.map TravelMsg (Travel.mountCmd id)]

                Pages.UserPage id ->
                    { model
                        | page = page
                    }
                        ! [ Cmd.map UserPageMsg (UserPage.mountCmd id)]


view : Model -> Html Msg
view model =
    div []
        [ viewPage model ]

viewPage : Model -> Html Msg
viewPage model =
    case model.page of
        Pages.Home ->
            App.map HomeMsg <| Home.view model.homeModel

        Pages.TravelPage id ->
            App.map TravelMsg <| Travel.view model.travelModel

        Pages.UserPage id ->
            App.map UserPageMsg <| UserPage.view model.userPageModel

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
