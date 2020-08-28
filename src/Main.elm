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
            let
                addOperatorResult =
                    addOperator model.heldValue model.currInput operator
                        |> (\result ->
                                case result of
                                    Ok newHeldValue ->
                                        ( newHeldValue, "" )

                                    Err message ->
                                        ( model.heldValue, message )
                           )
            in
            { model
                | heldValue = Tuple.first addOperatorResult
                , currInput = ""
                , error = Tuple.second addOperatorResult
            }

        -- Remove duplication
        Calculate ->
            let
                calcOpResult =
                    calcOp model.heldValue model.currInput
                        |> (\result ->
                                case result of
                                    Ok newHeldValue ->
                                        ( newHeldValue, "" )

                                    Err message ->
                                        ( model.heldValue, message )
                           )
            in
            { model
                | heldValue = Tuple.first calcOpResult
                , currInput = ""
                , error = Tuple.second calcOpResult
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
                    Ok (Value (calculate previousInput operator newInput))

                None ->
                    Ok (PartialOperation newInput operator)

                PartialOperation v o ->
                    Err "Cannot operate on a value operator pair"

        Nothing ->
            -- Mising valid case:
            -- If HeldValue is "Value" and we are adding an op then we will have Partial Op
            Err "Could not parse input"



-- Remove duplication


calcOp : HeldValue -> String -> Result String HeldValue
calcOp currentHeldValue currentInput =
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
                    Ok (Value (calculate v o newInput))

        Nothing ->
            Err "Could not parse input"


calculate : Int -> Operator -> Int -> Int
calculate previousValue operator currentInput =
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
            [ input [ Attr.value model.currInput, Attr.style "text-align" "right" ] []
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
