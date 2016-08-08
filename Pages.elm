module Pages exposing (..)

import UrlParser exposing (Parser, parse, (</>), format, int, oneOf, s, string)
import Navigation exposing (Location)
import Html exposing (Html, Attribute, a)
import Html.Attributes exposing (href)

import String


toHash : Page -> String
toHash page =
    case page of
        HomePage ->
            "#homepage"

        TravelPage id ->
            "#travelpage/" ++ toString id

        UserPage ->
            "#userpage"

        UserUpdatePage ->
            "#userupdatepage"

hashParser : Navigation.Location -> Result String Page
hashParser location =
    UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)


type Page
    = HomePage
    | TravelPage Int
    | UserPage
    | UserUpdatePage


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format HomePage (s "homepage")
        , format TravelPage (s "travelpage" </> int)
        , format UserPage (s "userpage")
        , format UserUpdatePage (s "userupdatepage")
        ]


urlParser : Navigation.Parser (Result String Page)
urlParser =
    Navigation.makeParser hashParser


navigate : Page -> Cmd msg
navigate page =
    Navigation.newUrl (toHash page)


modify : Page -> Cmd msg
modify page =
    Navigation.modifyUrl (toHash page)


linkTo : Page -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo page attrs content =
    a ( [ href (toHash page) ] ++ attrs) content
