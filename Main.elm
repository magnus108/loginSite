module Main exposing (..)

import Navigation
import Pages
import HomePage
import TravelPage
import UserPage
import UserUpdatePage
import Html.App as App
import Html exposing (Html, Attribute, div, text, ul, li)


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
    { userId : Int
    , currentPage : Pages.Page
    , homePageModel : HomePage.Model
    , travelPageModel : TravelPage.Model
    , userPageModel : UserPage.Model
    , userUpdatePageModel : UserUpdatePage.Model
    }


initialModel : Model
initialModel =
    { userId = 1
    , currentPage = Pages.HomePage
    , homePageModel = HomePage.init
    , travelPageModel = TravelPage.init
    , userPageModel = UserPage.init
    , userUpdatePageModel = UserUpdatePage.init
    }


init : Result String Pages.Page -> ( Model, Cmd Msg )
init result =
    urlUpdate result initialModel


type Msg
    = NoOp
    | HomePageMsg HomePage.Msg
    | TravelPageMsg TravelPage.Msg
    | UserPageMsg UserPage.Msg
    | UserUpdatePageMsg UserUpdatePage.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomePageMsg m ->
            let
                ( subMdl, subCmd ) =
                    HomePage.update m model.homePageModel
            in
                { model | homePageModel = subMdl }
                    ! [ Cmd.map HomePageMsg subCmd ]

        TravelPageMsg m ->
            let
                ( subMdl, subCmd ) =
                    TravelPage.update m model.travelPageModel
            in
                { model | travelPageModel = subMdl }
                    ! [ Cmd.map TravelPageMsg subCmd ]

        UserPageMsg m ->
            let
                ( subMdl, subCmd ) =
                    UserPage.update m model.userPageModel
            in
                { model | userPageModel = subMdl }
                    ! [ Cmd.map UserPageMsg subCmd ]

        UserUpdatePageMsg m ->
            let
                ( subMdl, subCmd ) =
                    UserUpdatePage.update m model.userUpdatePageModel
            in
                { model | userUpdatePageModel = subMdl }
                    ! [ Cmd.map UserUpdatePageMsg subCmd ]
        _ ->
            model ! []


urlUpdate : Result String Pages.Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Err _ ->
            model ! [ Pages.modify model.currentPage ]

        Ok page ->
            case page of
                Pages.HomePage ->
                    { model
                        | currentPage = page
                    }
                        ! [ Cmd.map HomePageMsg HomePage.mountCmd ]

                Pages.TravelPage id ->
                    { model
                        | currentPage = page
                    }
                        ! [ Cmd.map TravelPageMsg (TravelPage.mountCmd id)]

                Pages.UserPage ->
                    { model
                        | currentPage = page
                    }
                        ! [ Cmd.map UserPageMsg UserPage.mountCmd ]

                Pages.UserUpdatePage ->
                    { model
                        | currentPage = page
                    }
                        ! [ Cmd.map UserUpdatePageMsg UserUpdatePage.mountCmd ]


view : Model -> Html Msg
view model =
    div []
        [ ul []
            [ li []
                [ Pages.linkTo (Pages.UserPage) [] [ text "Userpage" ]
                , Pages.linkTo (Pages.HomePage) [] [ text "Homepage" ]
                ]
            ]
        , viewPage model ]


viewPage : Model -> Html Msg
viewPage model =
    case model.currentPage of
        Pages.HomePage ->
            App.map HomePageMsg <| HomePage.view model.homePageModel

        Pages.TravelPage id ->
            App.map TravelPageMsg <| TravelPage.view model.travelPageModel

        Pages.UserPage ->
            App.map UserPageMsg <| UserPage.view model.userPageModel

        Pages.UserUpdatePage ->
            App.map UserUpdatePageMsg <| UserUpdatePage.view model.userUpdatePageModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
