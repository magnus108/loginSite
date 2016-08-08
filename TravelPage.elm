module TravelPage exposing (..)

import Html exposing (Html, div, text, h3, ul, li, span)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task


type alias Model =
    { data : Data
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
    { status : String
    , destination : String
    }


emptyData : Data
emptyData =
    { people = [] }


emptyModel : Model
emptyModel =
    { data = emptyData
    , message = "Initiating"
    }


init : Model
init =
    emptyModel


type Msg
    = NoOp
    | Error String
    | Get Result


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        Get result ->
            { model
                | message = "This is your travel"
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
    div []
        [ ul [] (List.map travelView person.travels)
        ]

travelView : Travel -> Html Msg
travelView travel =
    li []
        [ span [] [ text (travel.status) ]
        , text travel.destination
        ]


mountCmd : Int -> Cmd Msg
mountCmd id =
    get id Error Get


baseUrl : String
baseUrl =
    "http://localhost:3000/graphql?raw"


get : Int -> (String -> a) -> (Result -> a) -> Cmd a
get id errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (query id))
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
        ("status" := JsonD.string)
        ("destination" := JsonD.string)


query : Int -> JsonE.Value
query id =
    JsonE.object
        [ ("query", JsonE.string ("{people(where:{email:\"Rey87@gmail.com\"})
            {travels(where:{id:" ++ (toString id) ++ "}){status destination}}}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0
