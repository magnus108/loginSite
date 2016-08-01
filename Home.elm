module Home exposing (..)

import Http
import Api exposing (..)
import Html exposing (Html, div, h1, text, ul, li, p, span)

type alias Model =
    { travels : List Travel
    , message : String
    }


initialModel : Model
initialModel =
    { travels = []
    , message = "Initiating"
    }


init : Model
init =
    initialModel


type Msg
    = NoOp
    | HandleTravelsRetrieved (List Travel)
    | FetchTravelsFailed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        HandleTravelsRetrieved travels ->
            { model | message = "These are your travels", travels = travels }
                ! []

        FetchTravelsFailed err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "hey" ]
        , p [] [ text model.message ]
        , ul [] (List.map travelRow model.travels)
        ]

travelRow : Travel -> Html Msg
travelRow travel =
    li []
        [ span [] [ text (toString travel.id) ]
        , text travel.name
        ]


mountCmd : Cmd Msg
mountCmd =
    Api.fetchTravels FetchTravelsFailed HandleTravelsRetrieved
