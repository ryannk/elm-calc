module Main exposing (main)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Clicked Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Clicked num ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [] []
            , button [] [ text "calc" ]
            ]
        , div []
            (List.range 1 3
                |> List.map viewNumberButton
            )
        , div []
            (List.range 4 6
                |> List.map viewNumberButton
            )
        , div []
            (List.range 7 9
                |> List.map viewNumberButton
            )

        -- [ button [] [ text "1" ], button [] [ text "2" ], button [] [ text "3" ] ]
        -- , div []
        --     [ button [] [ text "4" ], button [] [ text "5" ], button [] [ text "6" ] ]
        -- , div []
        --     [ button [] [ text "7" ], button [] [ text "8" ], button [] [ text "9" ] ]
        , div []
            [ viewNumberButton 0, button [] [ text "+" ], button [] [ text "-" ] ]
        ]


viewNumberButton : Int -> Html Msg
viewNumberButton num =
    button [ Events.onClick (Clicked num) ] [ text (String.fromInt num) ]
