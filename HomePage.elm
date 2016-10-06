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
    { message : String
    , groups : List Group
    }


type alias HomePageResult =
    { data : HomePageData
    }


type alias HomePageData =
    { person : Person
    }


type alias Person =
    { groups : List Group
    }

type alias Group =
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


emptyModel : Model
emptyModel =
    { message = "Initiating"
    , groups = []
    }

init : Model
init =
    emptyModel


type Msg
    = NoOp
    | Get HomePageResult
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
                , groups = result.data.person.groups
            } ! []

        Error err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []


view : Model -> Html Msg
view model =
    let
        { class } =
            MainCss.navbarNamespace

        groupView group =
            li []
                [ ul [ class [MainCss.Main2, MainCss.List, MainCss.Flex]] (List.map travelView group.travels)
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
            , ul [class [MainCss.List]] (List.map groupView model.groups)
            ]


baseUrl : String
baseUrl =
    "http://localhost:3000/graphql?raw"


get : String -> (String -> a) -> (HomePageResult -> a) -> Cmd a
get userId errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (query userId))
        , headers =
            [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson homePageResultDecoder
        |> Task.mapError toString
        |> Task.perform errorMsg msg


homePageResultDecoder : JsonD.Decoder HomePageResult
homePageResultDecoder =
    JsonD.object1 HomePageResult
        ("data" := homePageDataDecoder)


homePageDataDecoder : JsonD.Decoder HomePageData
homePageDataDecoder =
    JsonD.object1 HomePageData
        ("person" := personDecoder)


personDecoder : JsonD.Decoder Person
personDecoder =
    JsonD.object1 Person
        ("groups" := JsonD.list groupDecoder)


groupDecoder : JsonD.Decoder Group
groupDecoder =
    JsonD.object1 Group
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
            JsonE.string ("query($uuid: String!){person(uuid:$uuid){groups(uuid:$uuid){travels(uuid:$uuid){id destination status}}}}"))
          , ("variables",
            JsonE.string ("{\"uuid\": \"" ++ userId ++ "\"}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0


mountCmd : LoginResult -> Cmd Msg
mountCmd loginResult =
    get loginResult.data.loginPerson.email Error Get
