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
    { currInput : String
    , operator : Maybe String
    , prevInput : Maybe String
    }


init : Model
init =
    { currInput = ""
    , operator = Nothing
    , prevInput = Nothing
    }



-- UPDATE


type Msg
    = NumClicked Int
    | OperatorClicked String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NumClicked num ->
            { model | currInput = model.currInput ++ String.fromInt num }

        OperatorClicked operator ->
            { model
                | prevInput = Just model.currInput
                , currInput = ""
                , operator = Just operator
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ Attr.value model.currInput, Attr.style "text-align" "right" ] []
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
        , div []
            [ viewNumberButton 0, viewOperatorButton "+", viewOperatorButton "-" ]
        , div []
            [ text (Maybe.withDefault "" model.prevInput) ]
        ]


viewOperatorButton : String -> Html Msg
viewOperatorButton operatorText =
    button [ Events.onClick (OperatorClicked operatorText) ] [ text operatorText ]


viewNumberButton : Int -> Html Msg
viewNumberButton num =
    button [ Events.onClick (NumClicked num) ] [ text (String.fromInt num) ]
