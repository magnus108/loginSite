
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


navbarNamespace : Html.CssHelpers.Namespace String class id msg
navbarNamespace =
    withNamespace "navbar"


--colors
whiteColor = rgba 255 255 255

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
                    [ descendants
                        [ Css.Elements.li
                            [ children
                                [ Css.Elements.a
                                    [ color (whiteColor 0.6)
                                    , display inlineBlock
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
            , margin auto
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
            , descendants
                [ Css.Elements.li
                    [ display inlineBlock
                    , descendants
                        [ Css.Elements.span
                            [ property "display" "inline-block"
                            , property "vertical-align" "middle"
                            , marginRight (Css.em 2)
                            ]
                        ]
                    ]
                ]
            ]
        ]
