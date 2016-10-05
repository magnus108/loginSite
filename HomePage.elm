module HomePage exposing (..)

import Http
import Pages
import Html exposing (Html, p, img, div, header, footer, text, h3, ul, li, span)
import Html.Attributes exposing (..)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task

import MainCss

type alias Model =
    { userId : Maybe String
    , data : Data
    , message : String
    }


type alias Result =
    { data : Data
    }


type alias Data =
    { people : List Person
    }


type alias Person =
    { travels : List Travel
    }


type alias Travel =
    { id : Int
    , destination : String
    }

type alias LoginResult =
    --æregeligt den skal hede data
    { data : LoginData
    }

type alias LoginData =
    { loginPerson : Uuid
    }

type alias Uuid =
    { email : String
    }

emptyData : Data
emptyData =
    { people = [] }


emptyModel : Model
emptyModel =
    { userId = Nothing
    , data = emptyData
    , message = "Initiating"
    }

init : Model
init =
    emptyModel


type Msg
    = NoOp
    | Get Result
    | Error String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        Get result ->
            { model
                | message = "These are your travels"
                , data = result.data
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
            li []
                [ ul [ class [MainCss.Main2, MainCss.List, MainCss.Flex]] (List.map travelView person.travels)
                ]

        travelView travel =
            li [ class [MainCss.Card ]]
                [ img [src "http://cdn.buynowsignal.com/wp-content/uploads/sites/19/1-best-beach-umbrella-300x300.jpg"] []
                , div [class [MainCss.CardContent]]
                    [ header [] [ p [] [ text travel.destination] ]
                    , footer [] [ Pages.linkTo (Pages.TravelPage travel.id) [class [MainCss.Link]] [ text "More information"]]
                    ]
                ]
    in
    div []
        [ h3 [class [MainCss.Headline]] [ text model.message ]
        , ul [class [MainCss.List]] (List.map personView model.data.people)
        ]


baseUrl : String
baseUrl =
    "http://localhost:3000/graphql?raw"


get : String -> (String -> a) -> (Result -> a) -> Cmd a
get userId errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (query userId))
        , headers =
            [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson resultDecoder
        |> Task.mapError toString
        |> Task.perform errorMsg msg


resultDecoder : JsonD.Decoder Result
resultDecoder =
    JsonD.object1 Result
        ("data" := dataDecoder)


dataDecoder : JsonD.Decoder Data
dataDecoder =
    JsonD.object1 Data
        ("people" := peopleDecoder)


peopleDecoder : JsonD.Decoder (List Person)
peopleDecoder =
    JsonD.list personDecoder


personDecoder : JsonD.Decoder Person
personDecoder =
    JsonD.object1 Person
        ("travels" := JsonD.list travelDecoder)


travelDecoder : JsonD.Decoder Travel
travelDecoder =
    JsonD.object2 Travel
        ("id" := JsonD.int)
        ("destination" := JsonD.string)


query : String -> JsonE.Value
query userId =
    JsonE.object
        [ ("query",
            JsonE.string ("{people(where:{email:\"" ++ userId ++ "\"}){travels{id destination}}}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0


mountCmd : LoginResult -> Cmd Msg
mountCmd loginResult =
    get loginResult.data.loginPerson.email Error Get
