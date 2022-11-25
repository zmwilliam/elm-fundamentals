module Hello exposing (..)

import Browser
import Html exposing (Html, button, div, h3, img, table, tbody, text, th, thead, tr)
import Html.Attributes exposing (id, src, width)
import Html.Events exposing (onClick)


greeting : Int -> String
greeting count =
    "Welcome! you have " ++ String.fromInt count ++ " items in your cart"


type Msg
    = IncrementQuantities
    | DecrementQuantities
    | ResetQuantities


type alias Product =
    { name : String
    , photo : String
    , quantity : Int
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
    case msg of
        IncrementQuantities ->
            List.map (\p -> { p | quantity = p.quantity + 1 }) model

        DecrementQuantities ->
            List.map (\p -> { p | quantity = p.quantity - 1 }) model

        ResetQuantities ->
            productsModel


productsModel : Model
productsModel =
    [ { name = "Cheese", photo = "cheese.png", quantity = 1 }
    , { name = "Bananas", photo = "bananas.png", quantity = 3 }
    , { name = "Milk", photo = "milk.png", quantity = 2 }
    ]


itemView : Product -> Html msg
itemView model =
    tr []
        [ th [] [ text model.name ]
        , th [] [ img [ src model.photo, width 100 ] [] ]
        , th [] [ text <| String.fromInt model.quantity ]
        ]


cartView : Model -> Html Msg
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
            , tbody [] (List.map itemView model)
            ]
        , button [ onClick IncrementQuantities ] [ text "Increase quantities" ]
        , button [ onClick DecrementQuantities ] [ text "Decrease quantities" ]
        , button [ onClick ResetQuantities ] [ text "Reset quantities" ]
        ]


main =
    Browser.sandbox
        { init = productsModel
        , update = update
        , view = cartView
        }
