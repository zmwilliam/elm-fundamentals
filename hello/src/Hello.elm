module Hello exposing (..)

import Html exposing (Html, div, h3, img, table, tbody, text, th, thead, tr)
import Html.Attributes exposing (id, src, width)


greeting : Int -> String
greeting count =
    "Welcome! you have " ++ String.fromInt count ++ " items in your cart"


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


productsModel : List { name : String, photo : String, quantity : Int }
productsModel =
    [ { name = "Cheese", photo = "cheese.png", quantity = 1 }
    , { name = "Bananas", photo = "bananas.png", quantity = 3 }
    , { name = "Milk", photo = "milk.png", quantity = 2 }
    ]


itemView : { name : String, photo : String, quantity : Int } -> Html msg
itemView model =
    tr []
        [ th [] [ text model.name ]
        , th [] [ img [ src model.photo, width 100 ] [] ]
        , th [] [ text <| String.fromInt model.quantity ]
        ]


cartView : List { name : String, photo : String, quantity : number } -> Html msg
cartView model =
    div [ id "cart" ]
        [ h3 [] [ greeting 0 |> text ]
        , div [] [ dayPromo 1 |> text ]
        , div [] [ text <| fruitPromo "banana" ]
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Product" ]
                    , th [] [ text "Photo" ]
                    , th [] [ text "Quantity" ]
                    ]
                ]
            , tbody [] (List.map itemView productsModel)
            ]
        ]


main =
    cartView productsModel
