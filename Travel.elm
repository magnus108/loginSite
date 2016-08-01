module Travel exposing (..)

import Html exposing (Html, div, h1, text, ul, li, p, span)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task

type alias Travel =
    { id : Int
    , name : String
    , status : String
    }

type alias Model =
    { travel : Maybe Travel
    , message : String
    }


initialModel : Model
initialModel =
    { travel = Nothing
    , message = "Initiating"
    }


init : Model
init =
    initialModel


type Msg
    = NoOp
    | HandleTravelRetrieved Travel
    | FetchTravelFailed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        HandleTravelRetrieved travel ->
            { model | message = "Current status of travel", travel = (Just travel) }
                ! []

        FetchTravelFailed err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "hey" ]
        , p [] [ text model.message ]
        , (viewTravel model.travel)
        ]


viewTravel : Maybe Travel -> Html Msg
viewTravel travel =
    case travel of
        Nothing ->
            text ""

        Just t ->
            ul []
                [ li [] [ text (toString t.id) ]
                , li [] [ text (t.name) ]
                , li [] [ text (t.status) ]
                ]


mountCmd : Int -> Cmd Msg
mountCmd id =
    fetchTravel id FetchTravelFailed HandleTravelRetrieved


fetchTravel : Int -> (String -> msg) -> (Travel -> msg) -> Cmd msg
fetchTravel id errorMsg msg =
    Http.get travelDecoder ("http://localhost:3000/travel/" ++ toString id)
    |> Task.mapError toString
    |> Task.perform errorMsg msg


travelDecoder : JsonD.Decoder Travel
travelDecoder =
    JsonD.object3 Travel
        ("id" := JsonD.int)
        ("name" := JsonD.string)
        ("status" := JsonD.string)
