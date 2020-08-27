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


type Operator
    = Plus
    | Minus
    | Multiply
    | Divide


operatorToString : Operator -> String
operatorToString operator =
    case operator of
        Plus ->
            "+"

        Minus ->
            "-"

        Multiply ->
            "*"

        Divide ->
            "/"


operatorToSystemOperator : Operator -> (Int -> Int -> Int)
operatorToSystemOperator operator =
    case operator of
        Plus ->
            (+)

        Minus ->
            (-)

        Multiply ->
            (*)

        Divide ->
            (//)


type alias Model =
    { currInput : String
    , operator : Maybe Operator
    , prevOutput : Maybe Int
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
    | OperatorClicked Operator


update : Msg -> Model -> Model
update msg model =
    case msg of
        NumClicked num ->
            { model | currInput = model.currInput ++ String.fromInt num }

        OperatorClicked operator ->
            { model
                | prevOutput = determineHeldValue operator model -- Do better error handling
                , currInput = ""
                , operator = Just operator
            }


determineHeldValue : Operator -> Model -> Maybe Int
determineHeldValue operator model =
    case model.prevOutput of
        Just prevOutput ->
            calculate operator model prevOutput

        Nothing ->
            String.toInt model.currInput


calculate : Operator -> Model -> Int -> Maybe Int
calculate operator model value =
    let
        inputInt =
            model.currInput |> String.toInt

        systemOperator =
            operatorToSystemOperator operator
    in
    Maybe.map2 systemOperator inputInt model.prevOutput



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
            [ viewNumberButton 0, viewOperatorButton "+" Plus, viewOperatorButton "-" Minus ]
        , div []
            [ viewCurrentOperation model ]
        ]


viewCurrentOperation : Model -> Html Msg
viewCurrentOperation model =
    text
        ((model.operator |> Maybe.map operatorToString |> Maybe.withDefault "")
            ++ (model.prevOutput |> Maybe.map String.fromInt |> Maybe.withDefault "")
        )


viewOperatorButton : String -> Operator -> Html Msg
viewOperatorButton operatorText operatorType =
    button [ Events.onClick (OperatorClicked operatorType) ] [ text operatorText ]


viewNumberButton : Int -> Html Msg
viewNumberButton num =
    button [ Events.onClick (NumClicked num) ] [ text (String.fromInt num) ]


viewRow : Int -> Int -> Html Msg
viewRow start end =
    div []
        (List.range start end
            |> List.map viewNumberButton
        )
