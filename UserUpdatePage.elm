-- this file is reallyy bad
-- this file is reallyy bad
-- this file is reallyy bad
-- this file is reallyy bad
-- this file is reallyy bad
-- this file is reallyy bad
-- this file is reallyy bad
-- this file is reallyy bad


module UserUpdatePage exposing (..)

import Pages
import Html exposing (Html, p, div, text, form, input, h3, ul, li, span)
import Html.Attributes exposing (type', placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task

import MainCss


type alias Model =
    { person : Maybe Person
    , message : String
    , state: State
    }

type State
    = Query
    | Mutation

type alias UserPageResult =
    { data : UserPageData
    }

type alias UserUpdatePageResult =
    { data : UserUpdatePageData
    }

type alias UserPageData =
    { person : Person
    }

type alias UserUpdatePageData =
    { person : Person
    }

type alias Person =
    { firstname : String
    }


type alias LoginResult =
    -- ærgeligt den skal hede data
    -- ærgeligt den skal hede data
    { data : LoginData
    }

type alias LoginData =
    { loginPerson : Uuid
    }

type alias Uuid =
    { email : String
    }


emptyModel : Model
emptyModel =
    { person = Nothing
    , message = "Initiating"
    , state = Query
    }


init : Model
init =
    emptyModel


type Msg
    = NoOp
    | Error String
    | Get UserPageResult
    | Post UserUpdatePageResult
    | Submit Person
    | Input String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        Get result ->
            { model
                | message = "This is your account"
                , person = Just result.data.person
                , state = Query
            } ! []

        Post result ->
            --these are the same now that seems bad 
            --these are the same now that seems bad 
            --these are the same now that seems bad 
            --these are the same now that seems bad 
            { model
                | message = "This is your account"
                , person = Just result.data.person
                , state = Mutation
            } ! []

        Error err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []

        Input str ->
            let
                person = model.person
            in
                case person of
                    Nothing -> model ! []
                    Just person ->
                        { model | person = Just { person | firstname = str }} ! []

        Submit person ->
            { model | message = "Initiating update" }
                ! [post person Error Post]



view : Model -> Html Msg
view model =
    let
        { class } =
            MainCss.navbarNamespace


        personUpdateView person =
            case person of
                Nothing -> div [] []
                Just person ->
                    p [] [ text person.firstname ]

        personFormView person =
            case person of
                Nothing -> div [] []
                Just person ->
                    div []
                        [ form [ onSubmit (Submit person), class [MainCss.Form] ]
                            [ input [ type' "text"
                                , placeholder "firstname"
                                , onInput Input
                                , value person.firstname
                                , class [MainCss.Input]
                                ] []
                            , input [ type' "submit"
                                , value "Update"
                                , class [MainCss.Submit]
                                ] []
                            ]
                        ]
    in
        --- this is a shitty solution
        --- this is a shitty solution
        --- this is a shitty solution
        --- this is a shitty solution
        --- this is a shitty solution is should go to userpage/success or something i think
        case model.state of
            Query ->
                div []
                    [ h3 [class [MainCss.Headline]] [ text model.message ]
                    , personFormView model.person
                    ]
            Mutation ->
                div []
                    [ h3 [class [MainCss.Headline]] [ text model.message ]
                    , personUpdateView model.person
                    ]



mountCmd : LoginResult -> Cmd Msg
mountCmd loginResult =
    get loginResult.data.loginPerson.email Error Get



baseUrl : String
baseUrl =
    "http://localhost:3000/graphql?raw"


get : String -> (String -> a) -> (UserPageResult -> a) -> Cmd a
get userId errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (query userId))
        , headers =
            [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson userPageResultDecoder
        |> Task.mapError toString
        |> Task.perform errorMsg msg


post : Person -> (String -> a) -> (UserUpdatePageResult -> a) -> Cmd a
post person errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (mutation person.firstname))
        , headers = [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson userUpdatePageResultDecoder
        |> Task.mapError toString
        |> Task.perform errorMsg msg


userUpdatePageResultDecoder : JsonD.Decoder UserUpdatePageResult
userUpdatePageResultDecoder =
    JsonD.object1 UserUpdatePageResult
        ("data" := userUpdatePageDataDecoder)


userPageResultDecoder : JsonD.Decoder UserPageResult
userPageResultDecoder =
    JsonD.object1 UserPageResult
        ("data" := userPageDataDecoder)


userUpdatePageDataDecoder : JsonD.Decoder UserUpdatePageData
userUpdatePageDataDecoder =
    JsonD.object1 UserUpdatePageData
        ("updatePerson" := personDecoder)


userPageDataDecoder : JsonD.Decoder UserPageData
userPageDataDecoder =
    JsonD.object1 UserPageData
        ("person" := personDecoder)


personDecoder : JsonD.Decoder Person
personDecoder =
    JsonD.object1 Person
        ("firstname" := JsonD.string)


mutation : String -> JsonE.Value
mutation firstname =
    let
        userId = "Dulce_Jenkins@hotmail.com"
    in
    JsonE.object
        [ ("query",
            JsonE.string ("mutation($uuid:String!,$personId:String!,$values:JSON!){updatePerson(values:$values,personId:$personId,uuid:$uuid){firstname}}"))
          , ("variables",
            JsonE.string ("{\"uuid\":\"" ++ userId ++ "\",\"personId\":\"" ++ userId ++ "\",\"values\":{\"firstname\":\"" ++ firstname ++ "\"}}"))
        ]


query : String -> JsonE.Value
query userId =
    JsonE.object
        [ ("query",
            JsonE.string ("query($uuid:String!){person(uuid:$uuid){firstname}}"))
          , ("variables",
            JsonE.string ("{\"uuid\": \"" ++ userId ++ "\"}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0
