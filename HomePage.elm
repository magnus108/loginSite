module HomePage exposing (..)

import Http
import Pages
import Html exposing (Html, div, text, h3, ul, li, span)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task


type alias Model =
    { userId : String
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


emptyData : Data
emptyData =
    { people = [] }


emptyModel : String -> Model
emptyModel userId =
    { userId = userId
    , data = emptyData
    , message = "Initiating"
    }

init : String -> Model
init userId =
    emptyModel userId


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
    div []
        [ h3 [] [ text model.message ]
        , ul [] (List.map personView model.data.people)
        ]


personView : Person -> Html Msg
personView person =
    li []
        [ ul [] (List.map travelView person.travels)
        ]


travelView : Travel -> Html Msg
travelView travel =
    li []
        [ span [] [ text (toString travel.id) ]
        , text travel.destination
        , Pages.linkTo (Pages.TravelPage travel.id) [] [ text "More information" ]
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


mountCmd : String -> Cmd Msg
mountCmd userId =
    get userId Error Get
