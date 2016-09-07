module TravelPage exposing (..)

import Pages
import Html exposing (Html, p, div, text, h3, ul, li, span, img)
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Http
import Task

import MainCss

import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { userId : Maybe String
    , data : Data
    , message : String
    }


type alias Result =
    { data : Data
    }


type alias Data =
    { people : List Person
    }


type alias Person =
    { travels : List Travel
    }


type alias Travel =
    { status : String
    , destination : String
    }


emptyData : Data
emptyData =
    { people = [] }


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        Get result ->
            { model
                | message = "This is your travel"
                , data = result.data
            } ! []

        Error err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []


view : Model -> Html Msg
view model =
    let

        { class } =
            MainCss.navbarNamespace

        personView person =
            div []
                [ ul [ class [MainCss.List]] (List.map travelView person.travels)
                ]

        travelView travel =
            li [class [MainCss.IconList]]
                [ span [class [MainCss.Icon]] [ icon travel.status ]
                , p [class [MainCss.Flex]] [ Html.text travel.destination ]
                ]

        icon status =
            case status of
                _ ->
                    svg
                        [ version "1.1", Svg.Attributes.style "fill: currentColor;", viewBox "0 0 24 24"
                        ]
                        [ Svg.path [d "M0 0h24v25H0z", fill "none"] []
                        , circle [cx "12", cy "4", r "2"] []
                        , Svg.path [d "M19 13v-2c-1.54.02-3.09-.75-4.07-1.83l-1.29-1.43c-.17-.19-.38-.34-.61-.45-.01 0-.01-.01-.02-.01H13c-.35-.2-.75-.3-1.19-.26C10.76 7.11 10 8.04 10 9.09V15c0 1.1.9 2 2 2h5v5h2v-5.5c0-1.1-.9-2-2-2h-3v-3.45c1.29 1.07 3.25 1.94 5 1.95zm-6.17 5c-.41 1.16-1.52 2-2.83 2-1.66 0-3-1.34-3-3 0-1.31.84-2.41 2-2.83V12.1c-2.28.46-4 2.48-4 4.9 0 2.76 2.24 5 5 5 2.42 0 4.44-1.72 4.9-4h-2.07z"] []
                        ]

    in
        div []
            [ h3 [class [MainCss.Headline]] [ Html.text model.message ]
            , ul [class [MainCss.List]] (List.map personView model.data.people)
            ]

mountCmd : Maybe String -> Int -> Cmd Msg
mountCmd userId id =
    case userId of
        Nothing ->
            Pages.navigate Pages.UnauthorizedPage
        Just email ->
            get email id Error Get


baseUrl : String
baseUrl =
    "http://localhost:3000/graphql?raw"


get : String -> Int -> (String -> a) -> (Result -> a) -> Cmd a
get userId id errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (query userId id))
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
        ("travels" := JsonD.list travelDecoder)


travelDecoder : JsonD.Decoder Travel
travelDecoder =
    JsonD.object2 Travel
        ("status" := JsonD.string)
        ("destination" := JsonD.string)


query : String -> Int -> JsonE.Value
query userId id =
    JsonE.object
        [ ("query", JsonE.string ("{people(where:{email:\"" ++ userId ++ "\"})
            {travels(where:{id:" ++ (toString id) ++ "}){status destination}}}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0
