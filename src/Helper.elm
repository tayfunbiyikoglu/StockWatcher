module Helper exposing (formatDate, randomTuple, randomTuples, round_2_digit, negativeChange, formatRandomPair)

import Date exposing (Date(..))
import Time exposing (Time)
import Random exposing (..)
import DateFormat exposing (format)
import String


formatDate : Time -> String
formatDate time =
    let
        date =
            Date.fromTime time
    in
        format "%Y-%m-%d %H:%M:%S" date


round_to_digit : Float -> Float -> Float
round_to_digit decimal float =
    let
        num =
            (float * 10 ^ decimal) |> round
    in
        (toFloat num) / 100


round_2_digit : Float -> Float
round_2_digit =
    round_to_digit 2


negativeChange : String -> Bool
negativeChange changeText =
    String.left 1 changeText == "-"


randomTuples : Int -> Generator (List ( Float, Float ))
randomTuples count =
    list count <| pair (float 0 100) (float -1 1)


randomTuple : Generator ( Float, Float )
randomTuple =
    pair (float 0 100) (float -1 1)


formatRandomPair : ( Float, Float ) -> ( Float, String )
formatRandomPair ( randomPrice, randomChange ) =
    let
        formattedPrice =
            round_2_digit randomPrice

        formattedChange =
            round_2_digit randomChange

        changePercent =
            (100 * formattedChange) / formattedPrice

        changeText =
            (toString formattedChange) ++ " (" ++ (toString <| round_2_digit changePercent) ++ "%)"
    in
        ( formattedPrice, changeText )
