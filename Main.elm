module Main exposing (..)

import Navigation
import Pages
import LoginPage
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
    { currentPage : Pages.Page
    , loginPageModel : LoginPage.Model
    , homePageModel : HomePage.Model
    , travelPageModel : TravelPage.Model
    , userPageModel : UserPage.Model
    , userUpdatePageModel : UserUpdatePage.Model
    }


initialModel : Model
initialModel =
    { currentPage = Pages.LoginPage
    , loginPageModel = LoginPage.init
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
    | LoginPageMsg LoginPage.Msg
    | HomePageMsg HomePage.Msg
    | TravelPageMsg TravelPage.Msg
    | UserPageMsg UserPage.Msg
    | UserUpdatePageMsg UserUpdatePage.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        LoginPageMsg m ->
            let
                ( subMdl, subCmd ) =
                    LoginPage.update m model.loginPageModel
            in
                { model | loginPageModel = subMdl }
                    ! [ Cmd.map LoginPageMsg subCmd ]

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


urlUpdate : Result String Pages.Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Err _ ->
            model ! [ Pages.modify model.currentPage ]

        Ok page ->
            let
                userId =
                    List.foldl (\person acc -> Just person.email ) Nothing model.loginPageModel.data.people
            in
                case page of
                    Pages.LoginPage ->
                        { model
                            | currentPage = page
                        }
                            ! []

                    Pages.HomePage ->
                        { model
                            | currentPage = page
                        }
                            ! [ Cmd.map HomePageMsg (HomePage.mountCmd userId) ]

                    Pages.TravelPage id ->
                        { model
                            | currentPage = page
                        }
                            ! [ Cmd.map TravelPageMsg (TravelPage.mountCmd userId id)]

                    Pages.UserPage ->
                        { model
                            | currentPage = page
                        }
                            ! [ Cmd.map UserPageMsg (UserPage.mountCmd userId ) ]

                    Pages.UserUpdatePage ->
                        { model
                            | currentPage = page
                        }
                            ! [ Cmd.map UserUpdatePageMsg (UserUpdatePage.mountCmd userId) ]


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


-- viewAUTHENTICATE????
-- viewAUTHENTICATE????
-- viewAUTHENTICATE????
-- viewAUTHENTICATE????
-- viewAUTHENTICATE????
-- viewAUTHENTICATE????
-- viewAUTHENTICATE????
-- viewAUTHENTICATE????
-- viewAUTHENTICATE????
-- viewAUTHENTICATE????
-- viewAUTHENTICATE????
-- viewAUTHENTICATE????

viewPage : Model -> Html Msg
viewPage model =
    case model.currentPage of
        Pages.LoginPage ->
            App.map LoginPageMsg <| LoginPage.view model.loginPageModel

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
