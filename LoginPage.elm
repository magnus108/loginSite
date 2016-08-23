module LoginPage exposing (..)

import Pages
import Html exposing (Html, Attribute, div, text, form, input, h3, ul, li)
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
    { email : String
    }

emptyPerson : Person
emptyPerson =
    { email = "" }

emptyData : Data
emptyData =
    { people = [emptyPerson] }


emptyModel : Model
emptyModel =
    { data = emptyData
    , message = "Login"
    }

init : Model
init =
    emptyModel


type Msg
    = NoOp
    | Error String
    | Get Result
    | Submit Person
    | Email String String

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
            } ! [ Pages.navigate Pages.HomePage ]

        Error err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []

        Email email str ->
            let
                data =
                    model.data

                (people, cmds) =
                    List.unzip
                        ( List.map (updateHelp email str) data.people )

            in
                {model | data = { data | people = people}} ! cmds


        Submit person ->
            { model | message = "Initiating update" }
                ! [post person Error Get]


updateHelp : String -> String -> Person -> (Person, Cmd Msg)
updateHelp email str person =
    if person.email /= email then
        person
            ! []
    else
        { person | email = str }
            ! []

view : Model -> Html Msg
view model =
    let
        head =
            h3 [] [ text model.message ]

        body =
            div []
                [ ul [] (List.map loginFormView model.data.people)
                ]
    in
        div [] [ head, body ]


loginFormView : Person -> Html Msg
loginFormView person =
    li []
        [ form [ onSubmit (Submit person) ]
            [ input [ type' "text"
                , placeholder "email"
                , onInput (Email person.email)
                , value person.email
                ] []
            , input [ type' "submit"
                , value "Login"
                ] []
            ]
        ]


baseUrl : String
baseUrl =
    "http://localhost:3000/graphql?raw"


post : Person -> (String -> a) -> (Result -> a) -> Cmd a
post person errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (query person.email))
        , headers = [ ( "Content-Type", "application/json" ) ]
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
        ("email" := JsonD.string)


query : String -> JsonE.Value
query userId =
    JsonE.object
        [ ("query", JsonE.string ("{people(where:{email:\"" ++ userId ++ "\"})
            {email}}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0
