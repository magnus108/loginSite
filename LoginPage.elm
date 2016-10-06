module LoginPage exposing (..)

import Pages
import Html exposing (Html, Attribute, div, text, form, input, h3, ul, li)
import Html.Attributes exposing (type', placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task

import MainCss

type alias Model =
    { loginResult : Maybe LoginResult
    , loginForm : LoginForm
    , message : String
    }

type alias LoginResult =
    --ærgeligt den skal hede data
    { data : LoginData
    }

type alias LoginData =
    { loginPerson : Uuid
    }

type alias Uuid =
    { email : String
    }

type alias LoginForm =
    { email : String
    , groupId : String
    }

emptyLoginForm : LoginForm
emptyLoginForm =
    { email = ""
    , groupId = ""
    }

emptyModel : Model
emptyModel =
    { loginResult = Nothing
    , loginForm = emptyLoginForm
    , message = "Login"
    }

init : Model
init =
    emptyModel

type Msg
    = NoOp
    | Error String
    | Get LoginResult
    | Submit LoginForm
    | Email String
    | GroupId String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        Get loginResult ->
            { model
                | message = "This is your account"
                , loginResult = Just loginResult
            } ! [ Pages.navigate Pages.HomePage ]

        Error err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []

        Email email ->
            let
                loginForm =
                    model.loginForm
            in
                {model | loginForm = { loginForm | email = email }} ! []

        GroupId groupId ->
            let
                loginForm =
                    model.loginForm
            in
                {model | loginForm = { loginForm | groupId = groupId }} ! []


        Submit loginForm ->
            { model | message = "Initiating update" }
                ! [post loginForm Error Get]


view : Model -> Html Msg
view model =
    let
        { class } =
            MainCss.navbarNamespace

        head =
            h3 [class [MainCss.Headline]] [ text model.message ]

        body =
            div [] [loginFormView model.loginForm]

        loginFormView loginForm =
            form [class [MainCss.Form], onSubmit (Submit loginForm) ]
                [ input [ type' "text"
                    , placeholder "email"
                    , onInput Email
                    , value loginForm.email
                    , class [MainCss.Input]
                    ] []
                , input [ type' "text"
                    , placeholder "groupId"
                    , onInput GroupId
                    , value loginForm.groupId
                    , class [MainCss.Input]
                    ] []
                , input [ type' "submit"
                    , value "Login"
                    , class [MainCss.Submit]
                    ] []
                ]

    in
        div [] [ head, body ]


baseUrl : String
baseUrl =
    "http://localhost:3000/graphql?raw"


post : LoginForm -> (String -> a) -> (LoginResult -> a) -> Cmd a
post loginForm errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (query loginForm.email loginForm.groupId))
        , headers = [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson loginResultDecoder
        |> Task.mapError toString
        |> Task.perform errorMsg msg


loginResultDecoder : JsonD.Decoder LoginResult
loginResultDecoder =
    JsonD.object1 LoginResult
        ("data" := loginDataDecoder)


loginDataDecoder : JsonD.Decoder LoginData
loginDataDecoder =
    JsonD.object1 LoginData
        ("loginPerson" := uuidDecoder)


uuidDecoder : JsonD.Decoder Uuid
uuidDecoder =
    JsonD.object1 Uuid
        ("uuid" := JsonD.string)


query : String -> String -> JsonE.Value
query email groupId =
    JsonE.object
        [ ("query", JsonE.string ("mutation{loginPerson(email:\"" ++ email ++ "\",groupId:" ++ groupId ++ "){uuid}}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0
