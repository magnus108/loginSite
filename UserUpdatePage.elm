module UserUpdatePage exposing (..)

import Html exposing (Html, div, text, form, input, h3, ul, li, span)
import Html.Attributes exposing (type', placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task


type alias UpdatePeople =
    { updatePeople : People
    }


type alias People =
    { people : List Person
    }

type Data
    = Query People
    | Mutation UpdatePeople


type alias Model =
    { data : Data
    , message : String
    }


type alias Result =
    { data : Data
    }


type alias Person =
    { firstname : String
    }


emptyData : Data
emptyData =
    Query { people = [] }


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
    | Submit Person
    | Input String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        Get result ->
            case result.data of
                Query data ->
                    { model
                        | message = "This is your account"
                        , data = Query data
                    } ! []
                Mutation data ->
                    { model
                        | message = "Your update"
                        , data = Mutation data
                    } ! []

        Error err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []

        Input firstname str ->
            case model.data of
                Query data ->
                    let
                        (people, cmds) =
                            List.unzip ( List.map (updateHelp upp firstname (Inputs str)) data.people )

                    in
                        { model | data = Query { data | people = people}} ! cmds

                Mutation data ->
                        model ! []

        Submit person  ->
            { model | message = "Initiating update" }
                ! [post person Error Get]


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
    let
        head =
            h3 [] [ text model.message ]

        body =
            case model.data of
                Query data ->
                    div []
                        [ ul [] (List.map personFormView data.people)
                        ]
                Mutation data ->
                    div []
                        [ ul [] (List.map personUpdateView data.updatePeople.people)
                        ]
    in
        div [] [ head, body ]


personUpdateView : Person -> Html Msg
personUpdateView person =
    li []
        [ text person.firstname
        ]


personFormView : Person -> Html Msg
personFormView person =
    li []
        [ form [ onSubmit (Submit person) ]
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
    get Error Get


baseUrl : String
baseUrl =
    "http://localhost:3000/graphql?raw"


get : (String -> a) -> (Result -> a) -> Cmd a
get errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode query)
        , headers =
            [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson resultDecoder
        |> Task.mapError toString
        |> Task.perform errorMsg msg


post : Person -> (String -> a) -> (Result -> a) -> Cmd a
post person errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (mutation person.firstname))
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
    JsonD.oneOf
    [ JsonD.object1 Query peopleDecoder
    , JsonD.object1 Mutation updatePeopleDecoder
    ]


updatePeopleDecoder : JsonD.Decoder UpdatePeople
updatePeopleDecoder =
    JsonD.object1 UpdatePeople
        ("updatePeople" := peopleDecoder)


peopleDecoder : JsonD.Decoder People
peopleDecoder =
    JsonD.object1 People
        ("people" := JsonD.list personDecoder)


personDecoder : JsonD.Decoder Person
personDecoder =
    JsonD.object1 Person
        ("firstname" := JsonD.string)


mutation : String -> JsonE.Value
mutation firstname =
    JsonE.object
        [ ("query", JsonE.string ("mutation { updatePeople(values: {firstname: \"" ++ firstname ++ "\"},
            options: {where: {email: \"Rey87@gmail.com\"}, returning: true}) { people { firstname}}}"))
        ]


query : JsonE.Value
query =
    JsonE.object
        [ ("query", JsonE.string ("{people(where:{email:\"Rey87@gmail.com\"})
            {firstname}}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0
