module Main exposing (main)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import String



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = \model -> Element.layout [] (view model)
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
    | UpdatedField String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NumClicked num ->
            { model | currInput = model.currInput ++ String.fromInt num }

        OperatorClicked operator ->
            changeHeldValueHandler model (\m -> addOperator m.heldValue m.currInput operator)

        Calculate ->
            changeHeldValueHandler model (\m -> calculateEvent m.heldValue m.currInput)

        UpdatedField value ->
            { model
                | currInput = value
            }


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


view : Model -> Element Msg
view model =
    column [ spacing 8 ]
        [ el [] (viewCurrentOperation model)
        , row []
            [ Input.text []
                { onChange = UpdatedField
                , text = model.currInput
                , placeholder = Nothing
                , label = Input.labelHidden "number input"
                }
            , Input.button [] { onPress = Just Calculate, label = text "calc" }
            ]
        , viewRow 1 3
        , viewRow 4 6
        , viewRow 7 9
        , row [ spacing 8 ] [ viewNumberButton 0, viewOperatorButton "+" Plus, viewOperatorButton "-" Minus ]
        ]


viewCurrentOperation : Model -> Element Msg
viewCurrentOperation model =
    text (heldValueToString model.heldValue)


viewOperatorButton : String -> Operator -> Element Msg
viewOperatorButton operatorText operatorType =
    viewCalculatorButton
        { onPress = Just (OperatorClicked operatorType)
        , label = text operatorText
        }


viewNumberButton : Int -> Element Msg
viewNumberButton num =
    viewCalculatorButton
        { onPress = Just (NumClicked num)
        , label = text (String.fromInt num)
        }


viewCalculatorButton =
    Input.button
        [ Background.color (rgb255 0 100 200)
        , Font.color (rgb 1 1 1)
        , paddingXY 16 8
        , width (px 48)
        , mouseOver
            [ Background.color (rgb255 0 150 250)
            ]
        ]


viewRow : Int -> Int -> Element Msg
viewRow start end =
    row [ spacing 8 ]
        (List.range start end
            |> List.map viewNumberButton
        )
