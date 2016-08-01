module UserPage exposing (..)

import Html exposing (Html, div, form, input, h1, text, ul, li, p, span)
import Html.Attributes exposing (type', placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task


type alias User =
    { id : Int
    , email : String
    }


type alias Model =
    { user : Maybe User
    , message : String
    }


initialModel : Model
initialModel =
    { user = Nothing
    , message = "Initiating"
    }


init : Model
init =
    initialModel


type Msg
    = NoOp
    | Success User
    | Error String
    | Email String
    | Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        Success user ->
            { model | message = "Your user profile", user = Just user }
                ! []

        Error err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []

        Email s ->
            let
                user =
                    case model.user of
                        Nothing ->
                            Nothing

                        Just x ->
                            Just { x | email = s }
                in
                    { model | user = user }
                        ! []

        Update ->
            let
                cmds =
                    case model.user of
                        Nothing ->
                            []

                        Just x ->
                            [post x Error Success]
            in
                { model | message = "Initiating update" }
                    ! cmds

        _ ->
            model
                ! []


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "hey" ]
        , p [] [ text model.message ]
        , (viewUser model.user)
        ]


viewUser : Maybe User -> Html Msg
viewUser user =
    case user of
        Nothing ->
            text ""

        Just x ->
            form [ onSubmit Update ]
                [ input [ type' "text"
                        , placeholder "email"
                        , onInput Email
                        , value x.email
                        ] []
                , input [ type' "submit"
                        , value "Update"
                        ] []
                ]

mountCmd : Int -> Cmd Msg
mountCmd id =
    get id Error Success


get : Int -> (String -> msg) -> (User -> msg) -> Cmd msg
get id errorMsg msg =
    Http.get decode ("http://localhost:3000/users/" ++ toString id)
        |> Task.mapError toString
        |> Task.perform errorMsg msg


decode : JsonD.Decoder User
decode =
    JsonD.object2 User
        ("id" := JsonD.int)
        ("email" := JsonD.string)


post : User -> (String -> msg) -> (User -> msg) -> Cmd msg
post user errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = "http://localhost:3000/users"
        , body = Http.string (encode user)
        , headers = [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson decode
        |> Task.mapError toString
        |> Task.perform errorMsg msg

encode user =
    JsonE.encode 0
        <| JsonE.object
            [ ("id", JsonE.int user.id)
            , ("email", JsonE.string user.email)
            ]
