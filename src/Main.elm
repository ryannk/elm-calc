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
    , prevOutput : Maybe String
    }


init : Model
init =
    { currInput = ""
    , operator = Nothing
    , prevOutput = Nothing
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
                | prevOutput = Just model.currInput
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
        , viewRow 1 3
        , viewRow 4 6
        , viewRow 7 9
        , div []
            [ viewNumberButton 0, viewOperatorButton "+", viewOperatorButton "-" ]
        , div []
            [ viewCurrentOperation model ]
        ]


viewCurrentOperation : Model -> Html Msg
viewCurrentOperation model =
    text (Maybe.withDefault "" model.operator ++ Maybe.withDefault "" model.prevOutput)


viewOperatorButton : String -> Html Msg
viewOperatorButton operatorText =
    button [ Events.onClick (OperatorClicked operatorText) ] [ text operatorText ]


viewNumberButton : Int -> Html Msg
viewNumberButton num =
    button [ Events.onClick (NumClicked num) ] [ text (String.fromInt num) ]


viewRow : Int -> Int -> Html Msg
viewRow start end =
    div []
        (List.range start end
            |> List.map viewNumberButton
        )
