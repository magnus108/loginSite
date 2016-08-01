module Pages exposing (..)

import UrlParser exposing (Parser, parse, (</>), format, int, oneOf, s, string)
import Navigation exposing (Location)
import Html exposing (Html, Attribute, a)
import Html.Attributes exposing (href)

import String


toHash : Page -> String
toHash page =
    case page of
        Home ->
            "#home"


hashParser : Navigation.Location -> Result String Page
hashParser location =
    UrlParser.parse identity pageParser (String.dropLeft 1 location.hash)


type Page
    = Home


pageParser : Parser (Page -> a) a
pageParser =
    oneOf
        [ format Home (s "home")
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
