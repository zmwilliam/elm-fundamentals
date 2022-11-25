module Hello exposing (..)

import Browser
import Html exposing (Html, button, div, h3, img, node, table, tbody, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, id, src, style, width)
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
                    ]
                ]
            , tbody [] (List.map itemView model)
            ]
        , button [ onClick IncrementQuantities, class "btn btn-primary" ] [ text "Increase quantities" ]
        , button [ onClick DecrementQuantities, class "btn btn-secondary" ] [ text "Decrease quantities" ]
        , button [ onClick ResetQuantities, class "btn btn-dark" ] [ text "Reset quantities" ]
        ]


main =
    Browser.sandbox
        { init = productsModel
        , update = update
        , view = cartView
        }
