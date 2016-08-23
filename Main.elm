module Main exposing (..)

import Navigation
import Pages
import LoginPage
import LogoutPage
import HomePage
import TravelPage
import UserPage
import UserUpdatePage
import UnauthorizedPage
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
    , logoutPageModel : LogoutPage.Model
    , homePageModel : HomePage.Model
    , travelPageModel : TravelPage.Model
    , userPageModel : UserPage.Model
    , userUpdatePageModel : UserUpdatePage.Model
    , unauthorizedPageModel : UnauthorizedPage.Model
    }


initialModel : Model
initialModel =
    { currentPage = Pages.LoginPage
    , loginPageModel = LoginPage.init
    , logoutPageModel = LogoutPage.init
    , homePageModel = HomePage.init
    , travelPageModel = TravelPage.init
    , userPageModel = UserPage.init
    , userUpdatePageModel = UserUpdatePage.init
    , unauthorizedPageModel = UnauthorizedPage.init
    }


init : Result String Pages.Page -> ( Model, Cmd Msg )
init result =
    urlUpdate result initialModel


type Msg
    = NoOp
    | LoginPageMsg LoginPage.Msg
    | LogoutPageMsg LogoutPage.Msg
    | HomePageMsg HomePage.Msg
    | TravelPageMsg TravelPage.Msg
    | UserPageMsg UserPage.Msg
    | UserUpdatePageMsg UserUpdatePage.Msg
    | UnauthorizedPageMsg UnauthorizedPage.Msg

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

        LogoutPageMsg m ->
            let
                ( subMdl, subCmd ) =
                    LogoutPage.update m model.logoutPageModel
            in
                { model | logoutPageModel = subMdl }
                    ! [ Cmd.map LogoutPageMsg subCmd ]

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

        UnauthorizedPageMsg m ->
            let
                ( subMdl, subCmd ) =
                    UnauthorizedPage.update m model.unauthorizedPageModel
            in
                { model | unauthorizedPageModel = subMdl }
                    ! [ Cmd.map UnauthorizedPageMsg subCmd ]


urlUpdate : Result String Pages.Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Err _ ->
            model ! [ Pages.modify model.currentPage ]

        Ok page ->
            let
                userId =
                    List.foldl (\person acc -> if person.email /= "" then Just person.email else Nothing ) Nothing model.loginPageModel.data.people

            in
                case page of
                    Pages.LoginPage ->
                        { model
                            | currentPage = page
                        }
                            ! []

                    Pages.LogoutPage ->
                        { model
                            | currentPage = page
                            , loginPageModel = LoginPage.init
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

                    Pages.UnauthorizedPage ->
                        { model
                            | currentPage = page
                        }
                            ! []


view : Model -> Html Msg
view model =
    let
        head =
            case model.currentPage of
                Pages.LoginPage ->
                    div [] []

                Pages.LogoutPage ->
                    div [] []

                Pages.UnauthorizedPage ->
                    div [] []

                _ ->
                    div []
                        [ ul []
                            [ li []
                                [ Pages.linkTo (Pages.UserPage) [] [ text "Userpage" ]
                                , Pages.linkTo (Pages.HomePage) [] [ text "Homepage" ]
                                , Pages.linkTo (Pages.LogoutPage) [] [ text "Logoutpage" ]
                                ]
                            ]
                        ]

        body =
            div [] [ viewPage model ]
    in
        div [] [ head, body ]


viewPage : Model -> Html Msg
viewPage model =
    case model.currentPage of
        Pages.LoginPage ->
            App.map LoginPageMsg <| LoginPage.view model.loginPageModel

        Pages.LogoutPage ->
            App.map LogoutPageMsg <| LogoutPage.view model.logoutPageModel

        Pages.HomePage ->
            App.map HomePageMsg <| HomePage.view model.homePageModel

        Pages.TravelPage id ->
            App.map TravelPageMsg <| TravelPage.view model.travelPageModel

        Pages.UserPage ->
            App.map UserPageMsg <| UserPage.view model.userPageModel

        Pages.UserUpdatePage ->
            App.map UserUpdatePageMsg <| UserUpdatePage.view model.userUpdatePageModel

        Pages.UnauthorizedPage ->
            App.map UnauthorizedPageMsg <| UnauthorizedPage.view model.unauthorizedPageModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
