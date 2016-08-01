module Api exposing (..)

import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task


type alias Travel =
    { id : Int
    , name : String
    }


baseUrl : String
baseUrl =
    "http://localhost:3000"


fetchTravels : (String -> msg) -> (List Travel -> msg) -> Cmd msg
fetchTravels errorMsg msg =
    Http.get travelsDecoder (baseUrl ++ "/travels")
    |> Task.mapError toString
    |> Task.perform errorMsg msg


travelsDecoder : JsonD.Decoder (List Travel)
travelsDecoder =
    JsonD.list travelDecoder


travelDecoder : JsonD.Decoder Travel
travelDecoder =
    JsonD.object2 Travel
        ("id" := JsonD.int)
        ("name" := JsonD.string)
