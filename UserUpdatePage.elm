module UserUpdatePage exposing (..)

import Pages
import Html exposing (Html, div, text, form, input, h3, ul, li, span)
import Html.Attributes exposing (type', placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task

import MainCss

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
    { userId : Maybe String
    , data : Data
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
    { userId = Nothing
    , data = emptyData
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
    | SetUser (Maybe String)


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
                            List.unzip
                                ( List.map (updateHelp firstname str) data.people )

                    in
                        { model | data = Query { data | people = people}} ! cmds

                Mutation data ->
                        model ! []

        Submit person ->
            case model.userId of
                Nothing ->
                    { model | message = "Missing userId" }
                        ! []
                Just userId ->
                    { model | message = "Initiating update" }
                        ! [post userId person Error Get]

        SetUser userId ->
            { model | userId = userId } ! []

updateHelp : String -> String -> Person -> (Person, Cmd Msg)
updateHelp firstname str person =
    if person.firstname /= firstname then
        person
            ! []
    else
        { person | firstname = str }
            ! []

view : Model -> Html Msg
view model =
    let
        { class } =
            MainCss.navbarNamespace

        head =
            h3 [class [MainCss.Headline]] [ text model.message ]

        body =
            case model.data of
                Query data ->
                    div []
                        [ ul [class [MainCss.List]] (List.map personFormView data.people)
                        ]
                Mutation data ->
                    div []
                        [ ul [class [MainCss.List]] (List.map personUpdateView data.updatePeople.people)
                        ]

        personFormView person =
            li []
                [ form [ onSubmit (Submit person), class [MainCss.Form] ]
                    [ input [ type' "text"
                        , placeholder "firstname"
                        , onInput (Input person.firstname)
                        , value person.firstname
                        , class [MainCss.Input]
                        ] []
                    , input [ type' "submit"
                        , value "Update"
                        , class [MainCss.Submit]
                        ] []
                    ]
                ]

        personUpdateView person =
            li []
                [ text person.firstname
                ]

    in
        div [] [ head, body ]



mountCmd : Maybe String -> Cmd Msg
mountCmd userId =
    case userId of
        Nothing ->
            Pages.navigate Pages.UnauthorizedPage
        Just email ->
            Cmd.batch
                [ Task.perform identity identity (Task.succeed (SetUser userId))
                , get email Error Get
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


post : String -> Person -> (String -> a) -> (Result -> a) -> Cmd a
post userId person errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (mutation userId person.firstname))
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


mutation : String -> String -> JsonE.Value
mutation userId firstname =
    JsonE.object
        [ ("query", JsonE.string ("mutation { updatePeople(values: {firstname: \"" ++ firstname ++ "\"},
            options: {where: {email: \"" ++ userId ++ "\"}, returning: true}) { people { firstname}}}"))
        ]


query : String -> JsonE.Value
query userId =
    JsonE.object
        [ ("query", JsonE.string ("{people(where:{email:\"" ++ userId ++ "\"})
            {firstname}}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0
