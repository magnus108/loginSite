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
import NotFoundPage
import Html.App as App
import Html exposing (Html, Attribute, node, div, text, ul, li, nav)

import Html.Attributes exposing (style, type')
import Css exposing (..)
import Css.Namespace exposing (namespace)
import Css.Elements
import Html.CssHelpers exposing (withNamespace)

import MainCss

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
    , notFoundPageModel : NotFoundPage.Model
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
    , notFoundPageModel = NotFoundPage.init
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
    | NotFoundPageMsg NotFoundPage.Msg

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

        NotFoundPageMsg m ->
            let
                ( subMdl, subCmd ) =
                    NotFoundPage.update m model.notFoundPageModel
            in
                { model | notFoundPageModel = subMdl }
                    ! [ Cmd.map NotFoundPageMsg subCmd ]


urlUpdate : Result String Pages.Page -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    case result of
        Err _ ->
            { model
                | currentPage = Pages.NotFoundPage
            }
                ! [ Pages.modify Pages.NotFoundPage ]

        Ok page ->
            let
                userId =
                    List.foldl (\person acc -> if person.email /= "" then Just person.email else Nothing ) Nothing model.loginPageModel.data.people
            in
                case page of
                    Pages.LoginPage ->
                        case userId of
                            Nothing ->
                                { model
                                    | currentPage = page
                                }
                                    ! []
                            Just uid ->
                                { model
                                    | currentPage = page
                                }
                                    ! [Pages.navigate Pages.HomePage]

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

                    Pages.NotFoundPage ->
                        { model
                            | currentPage = page
                        }
                            ! []









view : Model -> Html Msg
view model =
    let

        compiled =
            compile MainCss.css
--- this should maybe go somewere too???
        { id, class } =
            MainCss.navbarNamespace

        head =
            case model.currentPage of
                Pages.LoginPage ->
                    div [] []

                Pages.LogoutPage ->
                    div [] []

                Pages.UnauthorizedPage ->
                    div [] []

----- AHHH FORKSEL HER om man er loget ind eller ej gør igen forskel for denne
                Pages.NotFoundPage ->
                    div [] []

                _ ->
                    let
--- this should maybe go somewere too???
                        links =
                            linkifier model.currentPage
                                [ (Pages.UserPage, "UserPage")
                                , (Pages.HomePage, "HomePage")
                                , (Pages.LogoutPage, "LogoutPage")
                                ]
                    in
                        nav [ id MainCss.Navbar ] [ ul [ class [MainCss.List] ] links ]
        body =
            div [ class [MainCss.Content] ] [ viewPage model ]
    in
        div [class [MainCss.Main]] [
            --begone filth
            node "style" [type' "text/css" ] [ text compiled.css ]
            , head, body ]


linkifier : Pages.Page -> List ( Pages.Page, String ) -> List (Html Msg)
linkifier currentPage xs =
    -- REAL ugly fix and only works for now since we dont have notfoundpage and unauthorizedpage with params.
    let
        { classList } =
            MainCss.navbarNamespace
    in
        List.map (\(page, content) ->
            let
                isActive = currentPage == page || page == Pages.LoginPage
                    && (not (List.member currentPage [Pages.NotFoundPage, Pages.UnauthorizedPage]))
            in
                li [ ]
                    [ Pages.linkTo page [classList [(MainCss.Active, isActive)]] [ text content ]
                    ]
        ) xs


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

        Pages.NotFoundPage ->
            App.map NotFoundPageMsg <| NotFoundPage.view model.notFoundPageModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
