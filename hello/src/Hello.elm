module Hello exposing (..)

import Browser
import Html exposing (Html, button, div, h3, img, input, node, table, tbody, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, disabled, id, src, style, type_, value, width)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import List exposing (product)


greeting : Int -> String
greeting count =
    "Welcome! you have " ++ String.fromInt count ++ " items in your cart"


type Msg
    = Quantities Int
    | AddToCart Product
    | ChangeQuantity Product String
    | SubmitOrder
    | ServerResponse (Result Http.Error String)


submitOrderCommand : Cmd Msg
submitOrderCommand =
    Http.get
        { url = "http://localhost:8000/src/server-data/order.json"
        , expect = Http.expectString ServerResponse
        }


orderDecoder : JD.Decoder OrderResponse
orderDecoder =
    JD.map2 OrderResponse
        (JD.field "orderId" JD.int)
        (JD.field "delivery" JD.string)


type alias OrderResponse =
    { orderNumber : Int
    , deliveryEstimate : String
    }


type alias Product =
    { name : String
    , photo : String
    , quantity : Int
    , unitPrice : Int
    }


type alias Model =
    { products : List Product
    , status : String
    }


dayPromo : number -> String
dayPromo day =
    if day == 1 then
        "Prices are 30% off!"

    else
        "Prices are 10% off!"


fruitPromo : String -> String
fruitPromo fruit =
    case fruit of
        "apple" ->
            "Apples are on sale today"

        "banana" ->
            "Great prices for bananas"

        _ ->
            "Large discounts"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        changeQuantity q =
            ( { model
                | products =
                    List.map (\p -> { p | quantity = p.quantity + q }) model.products
                        |> List.filter (\i -> i.quantity > 0)
              }
            , Cmd.none
            )
    in
    case msg of
        Quantities 2 ->
            changeQuantity 1

        Quantities 1 ->
            changeQuantity -1

        Quantities _ ->
            ( initialModel, Cmd.none )

        AddToCart p ->
            ( { model
                | products =
                    if List.any (\i -> i.name == p.name) model.products then
                        List.map
                            (\i ->
                                if i.name == p.name then
                                    { i | quantity = i.quantity + 1 }

                                else
                                    i
                            )
                            model.products

                    else
                        List.append model.products [ p ]
              }
            , Cmd.none
            )

        ChangeQuantity p v ->
            ( { model
                | products =
                    model.products
                        |> List.map
                            (\i ->
                                if i.name == p.name then
                                    { i | quantity = v |> String.toInt |> Maybe.withDefault 1 }

                                else
                                    i
                            )
                        |> List.filter (\i -> i.quantity > 0)
              }
            , Cmd.none
            )

        SubmitOrder ->
            ( model, submitOrderCommand )

        ServerResponse (Ok responseText) ->
            let
                decodingResult =
                    JD.decodeString orderDecoder responseText

                newStatus =
                    case decodingResult of
                        Ok response ->
                            "Your order id is " ++ String.fromInt response.orderNumber ++ ". " ++ response.deliveryEstimate

                        Err _ ->
                            "No order id, we'll call you latter "
            in
            ( { products = [], status = newStatus }, Cmd.none )

        ServerResponse (Err _) ->
            ( { model | status = "An error occorred, please try again later!" }, Cmd.none )


calculaPrice : { a | quantity : Int, unitPrice : Int } -> String
calculaPrice extensible =
    extensible.quantity * extensible.unitPrice |> String.fromInt


initialModel : Model
initialModel =
    { products =
        [ { name = "Cheese", photo = "cheese.png", quantity = 1, unitPrice = 7 }
        , { name = "Bananas", photo = "bananas.png", quantity = 3, unitPrice = 5 }
        , { name = "Milk", photo = "milk.png", quantity = 2, unitPrice = 3 }
        ]
    , status = "Please add products and submit your order!"
    }


inventoryModel : List Product
inventoryModel =
    [ { name = "Cheese", photo = "cheese.png", quantity = 1, unitPrice = 7 }
    , { name = "Bananas", photo = "bananas.png", quantity = 1, unitPrice = 5 }
    , { name = "Milk", photo = "milk.png", quantity = 1, unitPrice = 3 }
    , { name = "Bread", photo = "bread.png", quantity = 1, unitPrice = 4 }
    , { name = "Chips", photo = "chips.png", quantity = 1, unitPrice = 2 }
    ]


itemView : Product -> Html Msg
itemView product =
    tr []
        [ th [] [ text product.name ]
        , th [] [ img [ src product.photo, width 100 ] [] ]
        , th []
            [ input
                [ type_ "number"
                , value (String.fromInt product.quantity)
                , onInput (ChangeQuantity product)
                ]
                []
            ]
        , th [] [ text <| calculaPrice product ]
        ]


inventoryItemView : Product -> Html Msg
inventoryItemView product =
    div [ class "col" ]
        [ img [ src product.photo, width 100 ] []
        , div [] [ text product.name ]
        , div [] [ text ("$ " ++ String.fromInt product.unitPrice) ]
        , div []
            [ button [ class "btn btn-outline-primary", onClick (AddToCart product) ] [ text "Add to Cart" ] ]
        ]


cartView : Model -> Html Msg
cartView model =
    div [ id "cart" ]
        [ node "link"
            [ attribute "rel" "stylesheet"
            , attribute "href" "main.css"
            ]
            []
        , node "link"
            [ attribute "rel" "stylesheet"
            , attribute "href" "https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/css/bootstrap.min.css"
            , attribute "integrity" "sha384-rbsA2VBKQhggwzxH7pPCaAqO46MgnOM80zW1RWuH61DGLwZJEdK2Kadq2F9CUG65"
            , attribute "crossorigin" "anonymous"
            ]
            []
        , h3 [ style "font-size" "200%" ]
            [ List.map (\p -> p.quantity) model.products |> List.sum |> greeting |> text
            ]
        , div [ class "special-notice" ] [ text model.status ]
        , div [] [ text <| fruitPromo "banana" ]
        , table [ class "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Product" ]
                    , th [] [ text "Photo" ]
                    , th [] [ text "Quantity" ]
                    , th [] [ text "Price" ]
                    ]
                ]
            , tbody [] (List.map itemView model.products)
            ]
        , div []
            [ text "Total amount: $ "
            , List.map (\p -> p.unitPrice * p.quantity) model.products |> List.sum |> String.fromInt |> text
            ]
        , button [ onClick (Quantities 2), class "btn btn-primary" ] [ text "Increase quantities" ]
        , button [ onClick (Quantities 1), class "btn btn-secondary" ] [ text "Decrease quantities" ]
        , button [ onClick (Quantities 0), class "btn btn-dark" ] [ text "Reset quantities" ]
        , button
            [ class "btn btn-outline-primary"
            , disabled (List.length model.products == 0)
            , onClick SubmitOrder
            ]
            [ text "Place Order" ]
        , div [ class "container" ]
            [ div [ class "row text-centered" ]
                (List.map inventoryItemView inventoryModel)
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = initialApp
        , update = update
        , view = cartView
        , subscriptions = subscriptionsApp
        }


subscriptionsApp : Model -> Sub Msg
subscriptionsApp model =
    Sub.none


initialApp : () -> ( Model, Cmd Msg )
initialApp _ =
    ( initialModel, Cmd.none )
