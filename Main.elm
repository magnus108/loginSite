module Main exposing (..)

import Navigation

main : Program Never
main =
    Navigation.program (Navigation.makeParser Routes.decode)
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }


type alias Model =
    { route : Routes.Route
    }

initialModel : Model
initialModel =
    { route = Home
    , homeModel = Home.init
    }


init : Result String Route -> ( Model, Cmd Msg )
init result =
    urlUpdate result initialModel


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
