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
    { travel : Maybe Travel
    , message : String
    }


type alias TravelPageResult =
    { data : TravelPageData
    }


type alias TravelPageData =
    { travel : Travel
    }


type alias Travel =
    { status : String
    , destination : String
    }


type alias LoginResult =
    --æregeligt den skal hede data
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
    { travel = Nothing
    , message = "Initiating"
    }


init : Model
init =
    emptyModel


type Msg
    = NoOp
    | Error String
    | Get TravelPageResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model
                ! []

        Get result ->
            { model
                | message = "This is your travel"
                , travel = Just result.data.travel
            } ! []

        Error err ->
            { model | message = "Oops! An error occurred: " ++ err }
                ! []


view : Model -> Html Msg
view model =
    let

        { class } =
            MainCss.navbarNamespace

        travelView travel =
            case travel of
                Nothing -> div [] []
                Just travel ->
                    div [] [ span [class [MainCss.Icon]] [ icon travel.status ]
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
            , travelView model.travel
            ]

mountCmd : LoginResult -> Int -> Cmd Msg
mountCmd loginResult id =
    get loginResult.data.loginPerson.email id Error Get


baseUrl : String
baseUrl =
    "http://localhost:3000/graphql?raw"


get : String -> Int -> (String -> a) -> (TravelPageResult -> a) -> Cmd a
get userId id errorMsg msg =
    Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl
        , body = Http.string (encode (query userId id))
        , headers =
            [ ( "Content-Type", "application/json" ) ]
        }
        |> Http.fromJson travelPageResultDecoder
        |> Task.mapError toString
        |> Task.perform errorMsg msg


travelPageResultDecoder : JsonD.Decoder TravelPageResult
travelPageResultDecoder =
    JsonD.object1 TravelPageResult
        ("data" := travelPageDataDecoder)


travelPageDataDecoder : JsonD.Decoder TravelPageData
travelPageDataDecoder =
    JsonD.object1 TravelPageData
        ("travel" := travelDecoder)


travelDecoder : JsonD.Decoder Travel
travelDecoder =
    JsonD.object2 Travel
        ("status" := JsonD.string)
        ("destination" := JsonD.string)


query : String -> Int -> JsonE.Value
query userId id =
    JsonE.object
        [ ("query",
            JsonE.string ("query($uuid:String!,$travelId:Int!){travel(uuid:$uuid,travelId:$travelId){id destination status}}"))
          , ("variables",
            JsonE.string ("{\"uuid\":\"" ++ userId ++ "\", \"travelId\":" ++ ( toString id ) ++ "}"))
        ]


encode : JsonE.Value -> String
encode =
    JsonE.encode 0
