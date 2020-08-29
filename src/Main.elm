module Main exposing (main)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick)
import String



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


type HeldValue
    = Value Int
    | PartialOperation Int Operator
    | None


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


heldValueToString : HeldValue -> String
heldValueToString heldValue =
    case heldValue of
        Value i ->
            String.fromInt i

        PartialOperation i op ->
            String.fromInt i ++ "   " ++ operatorToString op

        None ->
            " "


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
    , heldValue : HeldValue
    , error : String
    }


init : Model
init =
    { currInput = ""
    , heldValue = None
    , error = ""
    }



-- UPDATE


type Msg
    = NumClicked Int
    | OperatorClicked Operator
    | Calculate


update : Msg -> Model -> Model
update msg model =
    case msg of
        NumClicked num ->
            { model | currInput = model.currInput ++ String.fromInt num }

        OperatorClicked operator ->
            changeHeldValueHandler model (\m -> addOperator m.heldValue m.currInput operator)

        Calculate ->
            changeHeldValueHandler model (\m -> calculateEvent m.heldValue m.currInput)


changeHeldValueHandler : Model -> (Model -> Result String HeldValue) -> Model
changeHeldValueHandler currModel heldValueEvaluator =
    let
        newHeldValueAndError =
            heldValueEvaluator currModel
                |> (\result ->
                        case result of
                            Ok newHeldValue ->
                                ( newHeldValue, "" )

                            Err message ->
                                ( currModel.heldValue, message )
                   )
    in
    { currModel
        | heldValue = Tuple.first newHeldValueAndError
        , currInput = ""
        , error = Tuple.second newHeldValueAndError
    }


addOperator : HeldValue -> String -> Operator -> Result String HeldValue
addOperator currentHeldValue currentInput operator =
    let
        parsedInput =
            String.toInt currentInput
    in
    case parsedInput of
        Just newInput ->
            case currentHeldValue of
                Value previousInput ->
                    Ok (Value (evaluate previousInput operator newInput))

                None ->
                    Ok (PartialOperation newInput operator)

                PartialOperation _ _ ->
                    Err "Cannot operate on a value operator pair"

        Nothing ->
            case currentHeldValue of
                Value previousInput ->
                    Ok (PartialOperation previousInput operator)

                None ->
                    Err "Could not parse input"

                PartialOperation _ _ ->
                    Err "Could not parse input"



-- Remove duplication


calculateEvent : HeldValue -> String -> Result String HeldValue
calculateEvent currentHeldValue currentInput =
    let
        parsedInput =
            String.toInt currentInput
    in
    case parsedInput of
        Just newInput ->
            case currentHeldValue of
                Value _ ->
                    Err "What operator should be used?"

                None ->
                    Err "I have nothing to calculate against"

                PartialOperation v o ->
                    Ok (Value (evaluate v o newInput))

        Nothing ->
            Err "Could not parse input"


evaluate : Int -> Operator -> Int -> Int
evaluate previousValue operator currentInput =
    let
        systemOperator =
            operatorToSystemOperator operator
    in
    systemOperator previousValue currentInput



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ viewCurrentOperation model ]
        , div []
            [ input [ Attr.value model.currInput, Attr.style "text-align" "right" ] [] -- add on update for typed input
            , button [ Events.onClick Calculate ] [ text "calc" ]
            ]
        , viewRow 1 3
        , viewRow 4 6
        , viewRow 7 9
        , div []
            [ viewNumberButton 0, viewOperatorButton "+" Plus, viewOperatorButton "-" Minus ]
        ]


viewCurrentOperation : Model -> Html Msg
viewCurrentOperation model =
    text (heldValueToString model.heldValue)


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
