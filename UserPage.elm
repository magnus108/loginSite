module UserPage exposing (..)

import Pages
import Html exposing (Html, p, div, text, h3, ul, li, span)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task

import MainCss

type alias Model =
    { person : Maybe Person
    , message : String
    }


type alias UserPageResult =
    { data : UserPageData
    }


type alias UserPageData =
    { person : Person
    }

type alias Person =
    { firstname : String
    }

type alias LoginResult =
    { data : LoginData
    --ærgeligt den skal hede data
    }

type alias LoginData =
    { loginPerson : Uuid
    }

type alias Uuid =
    { email : String
    }


emptyModel : Model
emptyModel =
    { person = Nothing
    , message = "Initiating"
    }


init : Model
init =
    emptyModel


type Msg
    = NoOp
    | Error String
    | Get UserPageResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        Get result ->
            { model
                | message = "This is your account"
                , person = Just result.data.person
            } ! []

        Error err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []


view : Model -> Html Msg
view model =
    let
        { class } =
            MainCss.navbarNamespace

        personView person =
            case person of
                Nothing -> div [] []
                Just person ->
                    div []
                        [ p [] [ text person.firstname]
                        , p [] [ Pages.linkTo (Pages.UserUpdatePage) [class [MainCss.Link]] [ text "update" ] ]
                        ]
    in
        div []
            [ h3 [class [MainCss.Headline]] [ text model.message ]
            , personView model.person
            ]


mountCmd : LoginResult -> Cmd Msg
mountCmd loginResult =
    get loginResult.data.loginPerson.email Error Get


baseUrl : String
baseUrl =
    "http://localhost:3000/graphql?raw"


get : String -> (String -> a) -> (UserPageResult -> a) -> Cmd a
get userId errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (query userId))
        , headers =
            [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson userPageResultDecoder
        |> Task.mapError toString
        |> Task.perform errorMsg msg


userPageResultDecoder : JsonD.Decoder UserPageResult
userPageResultDecoder =
    JsonD.object1 UserPageResult
        ("data" := userPageDataDecoder)


userPageDataDecoder : JsonD.Decoder UserPageData
userPageDataDecoder =
    JsonD.object1 UserPageData
        ("person" := personDecoder)


personDecoder : JsonD.Decoder Person
personDecoder =
    JsonD.object1 Person
        ("firstname" := JsonD.string)


query : String -> JsonE.Value
query userId =
    JsonE.object
        [ ("query",
            JsonE.string ("query($uuid:String!){person(uuid:$uuid){firstname}}"))
          , ("variables",
            JsonE.string ("{\"uuid\": \"" ++ userId ++ "\"}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0
