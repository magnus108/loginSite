module UserPage exposing (..)

import Html exposing (Html, div, text, ul, li, p, span)
import Json.Decode as JsonD exposing ((:=))
import Pages
import Http
import Task


type alias Email
    = String


type alias UserId
    = Int


type alias Model =
    { emails : List Email
    , message : String
    , userId : UserId
    }


init : UserId -> Model
init userId =
    { emails = []
    , message = "Initiating"
    , userId = userId
    }


type Msg
    = NoOp
    | Success (List Email)
    | Error String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        Success emails ->
            { model | message = "Your user profile", emails = emails }
                ! []

        Error err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []

        _ ->
            model
                ! []


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text model.message ]
        , ul [] (List.map viewEmail model.emails)
        ]


viewEmail : Email -> Html Msg
viewEmail email =
    li []
        [ span [] [ Pages.linkTo (Pages.UserPage 1) [] [ text "Edit" ] ]
        , text email ]


mountCmd : UserId -> Cmd Msg
mountCmd id =
    get id Error Success


get : UserId -> (String -> msg) -> (List Email -> msg) -> Cmd msg
get userId errorMsg msg =
    Http.get decode ("http://localhost:3000/emails/")
        |> Task.mapError toString
        |> Task.perform errorMsg msg

decode : JsonD.Decoder (List String)
decode =
    JsonD.list JsonD.string
