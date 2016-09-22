module StockWatcher exposing (..)

import Html.App as App
import Time exposing (Time, second, every)
import Html exposing (..)
import Helper exposing (randomTuple, randomTuples, formatDate, round_2_digit, negativeChange, formatRandomPair)
import Html.Attributes
    exposing
        ( placeholder
        , class
        , type'
        , classList
        , disabled
        , src
        , style
        , value
        , min
        , max
        , for
        , id
        )
import Html.Events exposing (onInput, onClick, on, keyCode)
import String exposing (toUpper)
import Symbol exposing (isValid)
import Json.Decode as Json
import Random


type Msg
    = SymbolChanged String
    | AddSymbol
    | AddStock ( Float, Float )
    | DeleteStock Stock
    | NoOp
    | Tick Time
    | GetTimeSuccess Msg Time
    | GetTimeFailure String
    | UpdateRefreshRate String
    | RefreshStocks (List ( Float, Float ))


type alias Stock =
    { symbol : String
    , price : Float
    , change : String
    }


type alias Model =
    { symbol : String
    , stocks : List Stock
    , lastUpdate : Maybe Time
    , refreshRate : Float
    }


init : Model
init =
    { symbol = ""
    , stocks =
        []
    , lastUpdate = Nothing
    , refreshRate = 5
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update: " msg of
        SymbolChanged input ->
            { model | symbol = toUpper input } ! []

        AddSymbol ->
            model ! [ Random.generate AddStock Helper.randomTuple ]

        DeleteStock stock ->
            deleteStock stock model ! []

        NoOp ->
            model ! []

        Tick newTime ->
            { model | lastUpdate = Just newTime } ! [ Random.generate RefreshStocks (Helper.randomTuples (List.length model.stocks)) ]

        GetTimeSuccess _ time ->
            { model | lastUpdate = Just time } ! []

        GetTimeFailure msg ->
            { model | lastUpdate = Nothing } ! []

        UpdateRefreshRate rate ->
            { model | refreshRate = Result.withDefault 0 (String.toFloat rate) } ! []

        RefreshStocks listOfRandomTuples ->
            refreshStockPrices model listOfRandomTuples ! []

        AddStock randomPair ->
            addStock model randomPair ! []


refreshStockPrices : Model -> List ( Float, Float ) -> Model
refreshStockPrices model listOfRandomTuples =
    let
        oldStocks =
            model.stocks

        newStocks =
            List.map2 combineList oldStocks listOfRandomTuples
    in
        { model | stocks = newStocks }


combineList : Stock -> ( Float, Float ) -> Stock
combineList oldStock randomPair =
    let
        ( formattedPrice, changeText ) =
            Helper.formatRandomPair randomPair
    in
        { oldStock | price = formattedPrice, change = changeText }


addStock : Model -> ( Float, Float ) -> Model
addStock model randomPair =
    let
        stockExists =
            List.any (\s -> s.symbol == model.symbol) model.stocks

        ( formattedPrice, changeText ) =
            Helper.formatRandomPair randomPair

        stockToBeAdded =
            Stock model.symbol formattedPrice changeText

        ( newStocks, newSymbol ) =
            if stockExists then
                ( model.stocks, model.symbol )
            else
                ( stockToBeAdded :: model.stocks, "" )
    in
        { model | stocks = newStocks, symbol = newSymbol }


deleteStock : Stock -> Model -> Model
deleteStock stock model =
    let
        newStocks =
            List.filter (\s -> s.symbol /= stock.symbol) model.stocks
    in
        { model | stocks = newStocks }


view : Model -> Html Msg
view model =
    div [ class "pure-g" ]
        [ div [ class "pure-u-1-2" ]
            [ headerView
            , refreshRateView model
            , stocksView model
            , addSymbolForm model
            , lastUpdateMessageView model
            , hr [] []
            , p [] [ text <| toString model ]
            ]
        ]


lastUpdateMessageView : Model -> Html Msg
lastUpdateMessageView model =
    let
        lastUpdateTime =
            case model.lastUpdate of
                Nothing ->
                    ""

                Just val ->
                    Helper.formatDate val
    in
        if List.length model.stocks > 0 then
            p [] [ text ("Last update : " ++ lastUpdateTime) ]
        else
            p [] [ text "There is no Stock to update.. " ]


refreshRateView : Model -> Html Msg
refreshRateView model =
    div [ class "pure-form pure-form-aligned" ]
        [ fieldset []
            [ div [ class "pure-control-group" ]
                [ label
                    [ for "refreshRate"
                    ]
                    [ text "Refresh Rate" ]
                , input
                    [ id "refreshRate"
                    , type' "range"
                    , Html.Attributes.min "5"
                    , Html.Attributes.max "20"
                    , value <| toString model.refreshRate
                    , onInput UpdateRefreshRate
                    ]
                    []
                , label
                    [ style
                        [ ( "text-align", "left" )
                        , ( "padding-left", "10px" )
                        ]
                    ]
                    [ text <| toString model.refreshRate ++ " sn" ]
                ]
            ]
        ]


stockRowView : Int -> Stock -> Html Msg
stockRowView stripIndex stock =
    let
        stripClass =
            if stripIndex % 2 == 1 then
                "odd"
            else
                "even"
    in
        tr [ class <| "pure-table-" ++ stripClass ]
            [ td []
                [ text stock.symbol ]
            , td []
                [ text <| toString stock.price ]
            , td []
                [ label
                    [ classList
                        [ ( "negativeChange", negativeChange stock.change )
                        , ( "positiveChange", not <| negativeChange stock.change )
                        ]
                    ]
                    [ text stock.change ]
                ]
            , td []
                [ button
                    [ class "pure-button"
                    , onClick (DeleteStock stock)
                    ]
                    [ i [ class "fa fa-times" ]
                        []
                    ]
                ]
            ]


addSymbolForm : Model -> Html Msg
addSymbolForm model =
    let
        symbolIsInvalid =
            not (Symbol.isValid model.symbol)
    in
        div [ class "pure-form" ]
            [ fieldset []
                [ input
                    [ placeholder "Enter Symbol"
                    , onInput SymbolChanged
                    , onEnter AddSymbol
                    , type' "text"
                    , value model.symbol
                    , classList [ ( "input-invalid", symbolIsInvalid ) ]
                    ]
                    []
                , button
                    [ onClick AddSymbol
                    , classList [ ( "pure-button-disabled", symbolIsInvalid ), ( "pure-button", True ) ]
                    , disabled symbolIsInvalid
                    ]
                    [ text "Add" ]
                ]
            ]


headerView : Html Msg
headerView =
    h1 []
        [ img
            [ src "img/elm-logo.png"
            , style [ ( "height", "30px" ) ]
            ]
            []
        , text " Stock Watcher"
        ]


stocksView : Model -> Html Msg
stocksView model =
    div []
        [ table [ class "pure-table" ]
            [ thead []
                [ tr []
                    [ th []
                        [ text "Symbol" ]
                    , th []
                        [ text "Price" ]
                    , th []
                        [ text "Change" ]
                    , th []
                        [ text "Remove" ]
                    ]
                ]
            , tbody []
                (List.indexedMap stockRowView model.stocks)
            ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                NoOp
    in
        on "keydown" (Json.map tagger keyCode)


main : Program Never
main =
    App.program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    every (model.refreshRate * second) Tick
