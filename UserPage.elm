module UserPage exposing (..)

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
    { firstname : String
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
                | message = "This is your account"
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
        [ text person.firstname
        , span [] [ Pages.linkTo (Pages.UserUpdatePage) [] [ text "update" ] ]
        ]


mountCmd : String -> Cmd Msg
mountCmd userId =
    get userId Error Get


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
        ("firstname" := JsonD.string)


query : String -> JsonE.Value
query userId =
    JsonE.object
        [ ("query", JsonE.string ("{people(where:{email:\"" ++ userId ++ "\"}){firstname}}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0
