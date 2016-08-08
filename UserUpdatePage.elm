module UserUpdatePage exposing (..)

import Html exposing (Html, div, text, form, input, h3, ul, li, span)
import Html.Attributes exposing (type', placeholder, value)
import Html.Events exposing (onInput, onSubmit)
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
    { firstname : String
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
    | Fetch Result
    | Submit String
    | Input String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        Fetch result ->
            { model
                | message = "This is your account"
                , data = result.data
            } ! []

        Error err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []

        Input firstname str ->
            let
                (people, cmds) =
                    List.unzip ( List.map (updateHelp upp firstname (Inputs str)) model.data.people )

                data' =
                    model.data
            in
                { model | data = { data' | people = people}} ! cmds
        _ ->
            model
                ! []



updateHelp upp firstname bob person =
    if person.firstname /= firstname then
        (person, Cmd.none)
    else
        let
            (newperson, cmds) =
                upp bob person
        in
            ( newperson
            , Cmd.map (Input newperson.firstname) cmds
            )


type Bob
    = Inputs String

upp bob person =
    case bob of
        Inputs str ->
            { person | firstname = str }
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
        [ form [ onSubmit (Submit person.firstname) ]
            [ input [ type' "text"
                , placeholder "firstname"
                , onInput (Input person.firstname)
                , value person.firstname
                ] []
            , input [ type' "submit"
                , value "Update"
                ] []
            ]
        ]


mountCmd : Cmd Msg
mountCmd =
    fetch Error Fetch


baseUrl : String
baseUrl =
    "http://localhost:3000/graphql?raw"


fetch : (String -> a) -> (Result -> a) -> Cmd a
fetch errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (query))
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


query : JsonE.Value
query =
    JsonE.object
        [ ("query", JsonE.string ("{people(where:{email:\"Rey87@gmail.com\"})
            {firstname}}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0
