module MainCss exposing (..)

import Css exposing (..)
import Css.Namespace exposing (namespace)
import Css.Elements
import Html.CssHelpers exposing (withNamespace)
--this whole is to not have conficting namespaces in diffrent components

type CssClasses
    = Navbar
    | Active
    | Content
    | List
    | Headline
    | Icon
    | Flex
    | Link
    | Column
    | Main
    | Main2
    | Card
    | Input
    | Form
    | Submit
    | Body
    | CardContent
    | IconList

navbarNamespace : Html.CssHelpers.Namespace String class id msg
navbarNamespace =
    withNamespace "navbar"


--colors
whiteColor = rgba 255 255 255
blackColor = rgba 0 0 0

primary1 = rgba 0 150 136 1
primary2 = rgba 38 166 154 1

warning1 = rgba 254 86 86 1



css : Css.Stylesheet
css =
    (stylesheet << namespace navbarNamespace.name)
        [ (#) Navbar
            [ backgroundColor primary1
            , descendants
                [ Css.Elements.ul
                    [ displayFlex
                    , descendants
                         [ Css.Elements.li
                            [ children
                                [ Css.Elements.a
                                    [ color (whiteColor 0.6)
                                    , display inlineBlock
                                    , property "outline" "0"
                                    , textDecoration none
                                    , textTransform uppercase
                                    , property "font-family" "'Helvetica','Arial',sans-serif"
                                    , height (Css.px 48)
                                    , lineHeight (Css.px 48)
                                    , textAlign center
                                    , fontSize (Css.px 14)
                                    , padding (Css.em 1)
                                    , paddingRight (Css.em 3)
                                    , paddingLeft (Css.em 3)
                                    , fontWeight bold
                                    , (withClass Active)
                                        [ fontWeight bold
                                        , color (whiteColor 1)
                                        , property "pointer-events" "none"
                                        , borderBottom3 (Css.px 2) solid warning1
                                        , backgroundColor primary2
                                        ]
                                    ]
                                , Css.Elements.a
                                    [ hover
                                        [ backgroundColor primary2
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , (.) Content
            [ paddingLeft (Css.pct 8)
            , paddingRight (Css.pct 8)
            , property "font-family" "'Roboto','Helvetica','Arial',sans-serif"
            ]
        , (.) Headline
            [ property "font-size" "24px"
            , property "font-weight" "400"
            , property "line-height" "32px"
            , property "opacity" ".87"
            ]
        , (.) List
            [ margin (Css.em 0)
            , padding (Css.em 0)
            , property "list-style" "none"
            , children
                [ (.) IconList
                    [ displayFlex
                    , property "align-items" "center"
                    , color (blackColor 0.60)
                    ]
                ]
            ]
        , (.) Icon
            [ width (Css.em 2)
            , height (Css.em 2)
            , verticalAlign middle
            , display inlineBlock
            , marginRight (Css.em 2)
            ]
        , (.) Flex
            [ displayFlex
            ]
            -- this and Flex should have some shared attributes..
        , (.) Main
            [ flexFlow2 column wrap
            , displayFlex
            , fontSize (Css.px 16)
            ]
        , (.) Body
            [ color (blackColor 1)
            , property "font-family" "'Roboto','Helvetica','Arial',sans-serif"
            , fontSize (Css.pct 62.5)
            , descendants
                [ Css.Elements.p
                    [ fontSize (Css.em 1.4)
                    ]
                ]
            ]
        , (.) Link
            [ textDecoration none
            , textTransform uppercase
            ]
        , (.) Column
            [ flexDirection column
            ]
            --shittymain again
        , (.) Main2
            [ flexFlow2 row wrap
            ]
        , (.) Form
            [ displayFlex
            , flexDirection column
            , descendants
                [ (.) Input
                    [ borderStyle Css.none
                    , padding2 (Css.px 4) (Css.px 0)
                    , margin2 (Css.em 1) (Css.px 0)
                    , borderBottom3 (Css.px 1) solid (blackColor 0.12)
                    , property "outline" "0"
                    , property "transition" "border-bottom 0.4s"
                    , focus
                        [ borderBottom3 (Css.px 1) solid warning1
                        ]
                    ]
                , (.) Submit
                    [ padding2 (Css.px 16) (Css.em 1)
                    , margin2 (Css.em 1) (Css.em 0)
                    , textTransform uppercase
                    , fontSize (Css.px 14)
                    , borderStyle Css.none
                    , backgroundColor (whiteColor 1)
                    , borderRadius (Css.px 3)
                    , property "outline" "0"
                    , hover
                        [ backgroundColor (blackColor 0.05)
                        ]
                    ]
                ]
            ]
        , (.) Card
            [ property "box-shadow" "0 2px 2px 0 rgba(0,0,0,.14),0 3px 1px -2px rgba(0,0,0,.2),0 1px 5px 0 rgba(0,0,0,.12)"
            , displayFlex
            , flexFlow2 column wrap
            , flex2 (Css.int 0) (Css.int 1)
            , margin4 (Css.em 0) (Css.em 1) (Css.em 1) (Css.em 0)
            , property "word-break" "break-word"
            , descendants
                [ (.) CardContent
                    [ padding (Css.em 1)
                    , flex (Css.int 1)
                    , displayFlex
                    , flexFlow1 column
                    , children
                        [ Css.Elements.footer
                            [ displayFlex
                            , children
                                [ Css.Elements.a
                                    [ color warning1
                                    , flex (Css.int 1)
                                    , property "outline" "0"
                                    , textAlign center
                                    , fontSize (Css.px 14)
                                    , padding (Css.px 16)
                                    , hover
                                        [ borderRadius (Css.px 2)
                                        , backgroundColor (blackColor 0.05)
                                        ]
                                    ]
                                ]
                            ]
                        , Css.Elements.header
                            [ fontSize (Css.em 2)
                            , flex (Css.int 1)
                            ]
                        ]
                    ]
                ]
            ]
        ]
