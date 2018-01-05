module Main exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes exposing (attribute, class)
import Html.Events as Events
import Html.Keyed as Keyed
import Json.Decode as Json
import Random


-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { fortune = Nothing
      , count = 0
      }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { fortune : Maybe Fortune
    , count : Int
    }


type Fortune
    = Daikichi
    | Kichi
    | ShouKichi
    | SueKichi
    | Kyou



displayFortune : Fortune -> String
displayFortune fortune =
    case fortune of
        Daikichi ->
            "大吉"

        Kichi ->
            "吉"

        ShouKichi ->
            "小吉"

        SueKichi ->
            "吉"

        Kyou ->
            "凶"



-- UPDATE


type Msg
    = ClickOmikuji
    | ChangeFortune Fortune
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickOmikuji ->
            ( model
            , changeFortune
            )

        ChangeFortune fortune ->
            ( { model
                | fortune = Just fortune
              }
            , Cmd.none
            )

        Reset ->
            ( { model
                | fortune = Nothing
                , count = model.count + 1
              }
            , Cmd.none
            )


numSample : Int
numSample = 3


changeFortune : Cmd Msg
changeFortune =
    Random.generate ChangeFortune <|
        Random.map (List.sum >> (\n -> n // numSample) >> intToFortune) <| Random.list numSample <| Random.int 0 5


intToFortune : Int -> Fortune
intToFortune n =
    case n of
        0 ->
            Daikichi

        1 ->
            Kichi

        2 ->
            ShouKichi

        3 ->
            SueKichi

        _ ->
            Kyou

-- VIEW


view : Model -> Html Msg
view model =
    unique (toString model.count) <| pack_
        [ class "vh100"
        , class "column"
        ]
        [ Html.header
            [ class "wrap"
            , class "row"
            , class "justifyCenter"
            , class "header"
            ]
            [ wrappedText "Elm toy mikuji"
            ]
        , wrap_
            [ class "scrollY"
            , class "expanded"
            ]
            [ wrap
                [ pack_
                    [ class "sakura-chan"
                    , Events.onClick ClickOmikuji
                    , case model.fortune of
                        Nothing ->
                            class "inactive"

                        Just _ ->
                            class "active"
                    ]
                    [ ball_ 1
                        [ pack_
                            [ class "content"
                            , class "expandV"
                            , onClickWithoutPropergate Reset
                            ]
                            [ wrap_
                                [ class "row"
                                , class "justifyEnd"
                                ]
                                [ wrap
                                    [ Html.text "×"
                                    ]
                                ]
                            , wrap_
                                [ class "row"
                                , class "justifyCenter"
                                ]
                                [ wrap
                                    [ Html.text <| Maybe.withDefault "" <| Maybe.map displayFortune model.fortune
                                    ]
                                ]
                            ]
                        ]
                    , ball 2
                    , ball 3
                    , ball 4
                    , ball 5
                    , ball 6
                    ]
                ]
            ]
        ]


ball_ : Int -> List (Html Msg) -> Html Msg
ball_ n children =
    pack_
        [ class "ball"
        , class <| "ball-" ++ toString n
        ]
        children


ball : Int -> Html Msg
ball n = ball_ n []



-- Helper View functions


pack : List (Html msg) -> Html msg
pack =
    pack_ []


pack_ : List (Attribute msg) -> List (Html msg) -> Html msg
pack_ attrs children =
    Html.div
        (class "pack" :: attrs)
        children


wrap : List (Html msg) -> Html msg
wrap =
    wrap_ []


wrap_ : List (Attribute msg) -> List (Html msg) -> Html msg
wrap_ attrs children =
    Html.div
        (class "wrap" :: attrs)
        children


wrappedText : String -> Html msg
wrappedText str =
    wrap
        [ Html.text str
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Helper functions



-- Utility functions


ariaSelected : Bool -> Attribute Msg
ariaSelected b =
    attribute "aria-selected" <|
        if b then
            "true"
        else
            "false"


ariaHidden : Bool -> Attribute Msg
ariaHidden b =
    attribute "aria-hidden" <|
        if b then
            "true"
        else
            "false"


role : String -> Attribute Msg
role =
    attribute "role"


result : (l -> x) -> (r -> x) -> Result l r -> x
result onErr onOk res =
    case res of
        Err l ->
            onErr l

        Ok r ->
            onOk r


maybe : a -> (b -> a) -> Maybe b -> a
maybe def f =
    Maybe.withDefault def << Maybe.map f


unique : String -> Html msg -> Html msg
unique n child =
    Keyed.node "div"
        [ class "pack"
        ]
        [ ( toString n
          , child
          )
        ]


onClickWithoutPropergate : msg -> Attribute msg
onClickWithoutPropergate message =
    Events.onWithOptions "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.succeed message)
