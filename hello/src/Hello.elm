module Hello exposing (..)

import Browser
import Html exposing (Html, button, div, h3, img, input, node, table, tbody, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, id, src, style, type_, value, width)
import Html.Events exposing (onClick, onInput)


greeting : Int -> String
greeting count =
    "Welcome! you have " ++ String.fromInt count ++ " items in your cart"


type Msg
    = Quantities Int
    | AddToCart Product
    | ChangeQuantity Product String


type alias Product =
    { name : String
    , photo : String
    , quantity : Int
    , unitPrice : Int
    }


type alias Model =
    List Product


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


update : Msg -> Model -> Model
update msg model =
    let
        changeQuantity q =
            List.map (\p -> { p | quantity = p.quantity + q }) model
                |> List.filter (\i -> i.quantity > 0)
    in
    case msg of
        Quantities 2 ->
            changeQuantity 1

        Quantities 1 ->
            changeQuantity -1

        Quantities _ ->
            initialModel

        AddToCart p ->
            if List.any (\i -> i.name == p.name) model then
                List.map
                    (\i ->
                        if i.name == p.name then
                            { i | quantity = i.quantity + 1 }

                        else
                            i
                    )
                    model

            else
                List.append model [ p ]

        ChangeQuantity p v ->
            List.map
                (\i ->
                    if i.name == p.name then
                        { i | quantity = v |> String.toInt |> Maybe.withDefault 1 }

                    else
                        i
                )
                model
                |> List.filter (\i -> i.quantity > 0)


calculaPrice : { a | quantity : Int, unitPrice : Int } -> String
calculaPrice extensible =
    extensible.quantity * extensible.unitPrice |> String.fromInt


initialModel : Model
initialModel =
    [ { name = "Cheese", photo = "cheese.png", quantity = 1, unitPrice = 7 }
    , { name = "Bananas", photo = "bananas.png", quantity = 3, unitPrice = 5 }
    , { name = "Milk", photo = "milk.png", quantity = 2, unitPrice = 3 }
    ]


inventoryModel : Model
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
            [ List.map (\p -> p.quantity) model |> List.sum |> greeting |> text
            ]
        , div [ class "special-notice" ] [ dayPromo 1 |> text ]
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
            , tbody [] (List.map itemView model)
            ]
        , div []
            [ text "Total amount: $ "
            , List.map (\p -> p.unitPrice * p.quantity) model |> List.sum |> String.fromInt |> text
            ]
        , button [ onClick (Quantities 2), class "btn btn-primary" ] [ text "Increase quantities" ]
        , button [ onClick (Quantities 1), class "btn btn-secondary" ] [ text "Decrease quantities" ]
        , button [ onClick (Quantities 0), class "btn btn-dark" ] [ text "Reset quantities" ]
        , div [ class "container" ]
            [ div [ class "row text-centered" ]
                (List.map inventoryItemView inventoryModel)
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = cartView
        }
