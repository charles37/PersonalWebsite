module Main exposing (..)
import List.Extra

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Browser.Events
import Json.Decode
import Time
import Optics.Basic exposing (..)
import Optics.Core exposing (..)
import Platform.Cmd as Cmd
import List.Extra
import Maybe exposing (withDefault)

type alias Model =
    { snake : Snake
    , boxWidth : Int
    , boxHeight : Int
    }

snake_ : SimpleLens ls { a | snake : b } b
snake_  =
    lens .snake (\b a -> { b | snake = a })

type Direction
    = Up
    | Down
    | Left
    | Right


type alias Snake =
    { body : List Point
    , direction : Direction
    }

body_ : SimpleLens ls { a | body : b } b
body_ =
    lens .body (\b a -> { b | body = a })

direction_ : SimpleLens ls { a | direction : b } b
direction_ =
    lens .direction (\b a -> { b | direction = a })

type alias Point =
    { x : Int
    , y : Int
    }

type alias TextSection =
    { title : String
    , description : String
    , position : Point
    }

textSections : List TextSection
textSections =
    [ { title = "Resume"
      , description = "Move the snake here to access my resume."
      , position = { x = 0, y = 0 }
      }
    , { title = "GitHub"
      , description = "Move the snake here to access my GitHub profile."
      , position = { x = 19, y = 0 }
      }
    , { title = "Twitter"
      , description = "Move the snake here to see my latest tweets."
      , position = { x = 0, y = 19 }
      }
    , { title = "Blog"
      , description = "Move the snake here to read my articles."
      , position = { x = 19, y = 19 }
      }
    ]

init : Model
init =
    { snake =
        { body =
            [ { x = 0, y = 0 }
            , { x = 1, y = 0 }
            , { x = 2, y = 0 }
            ]
        , direction = Right
        }
    , boxWidth = 20
    , boxHeight = 20
    }

type Msg
    = ChangeDirection Direction
    | Move
    | ResizeWindow Int Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDirection newDirection ->
            ( over snake_ (assign direction_ newDirection) model , Cmd.none )
            |> Debug.log "newDirection"
        Move ->
            let
                newSnake = moveSnake model.snake
            in
            ( { model | snake = newSnake }
            , Cmd.none
            )
            |> Debug.log "newSnake"
        ResizeWindow width height ->
            ( { model | boxWidth = width, boxHeight = height }, Cmd.none )

moveSnake : Snake -> Snake
moveSnake snake =
    let
        head =
            List.head snake.body

        newHead =
            case head of
                Just point ->
                    case snake.direction of
                        Up ->
                            { point | y = point.y - 1 }

                        Down ->
                            { point | y = point.y + 1 }

                        Left ->
                            { point | x = point.x - 1 }

                        Right ->
                            { point | x = point.x + 1 }

                Nothing ->
                    { x = 0, y = 0 }
    in
    assign body_ (newHead :: (withDefault [] (List.Extra.init snake.body))) snake

windowSizeToBoxDimensions : Int -> Int -> ( Int, Int )
windowSizeToBoxDimensions width height =
    let
        boxWidth =
            width // 20

        boxHeight =
            height // 20
    in
    ( boxWidth, boxHeight )

subscriptions : Model -> Sub Msg
subscriptions _ =
   Sub.batch
       [ Browser.Events.onKeyDown
           (Json.Decode.map ChangeDirection directionDecoder)
       , Time.every 200 (\_ -> Move)
       , Browser.Events.onResize (\w h -> let ( bw, bh ) = windowSizeToBoxDimensions w h in ResizeWindow bw bh)
       ]

directionDecoder : Json.Decode.Decoder Direction
directionDecoder =
    Json.Decode.oneOf
        [ Json.Decode.field "keyCode" Json.Decode.int
            |> Json.Decode.andThen
                (\code ->
                    case code of
                        38 ->
                            Json.Decode.succeed Up

                        40 ->
                            Json.Decode.succeed Down

                        37 ->
                            Json.Decode.succeed Left

                        39 ->
                            Json.Decode.succeed Right

                        _ ->
                            Json.Decode.fail "Invalid keyCode"
                )
        ]

view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "height" "100vh"
        , style "width" "100vw"
        , style "background-color" "white"
        , style "position" "relative"
        ]
        [ div
            [ style "position" "absolute"
            , style "top" "50%"
            , style "left" "50%"
            , style "transform" "translate(-50%, -50%)"
            , style "text-align" "center"
            ]
            [ h1 [] [ text "Welcome to My Personal Website" ]
            , p [] [ text "I'm Benjamin Prevor, enthusiastic developer with experience in Haskell, NixOS, and more. Use the snake to navigate my website!" ]
            ]
        , div
            [ style "position" "relative"
            , style "width" (String.fromInt (model.boxWidth * 20) ++ "px")
            , style "height" (String.fromInt (model.boxHeight * 20) ++ "px")
            , style "border" "1px solid black"
            ]
            (List.map viewSegment model.snake.body ++ List.map viewTextSection textSections)
        ]

viewTextSection : TextSection -> Html Msg
viewTextSection textSection =
    let
        xPos =
            if textSection.position.x == 0 then
                "10px"
            else
                "calc(100% - 10px - 20px)"

        yPos =
            if textSection.position.y == 0 then
                "10px"
            else
                "calc(100% - 10px - 20px)"
    in
    div
        [ style "position" "absolute"
        , style "width" "20px"
        , style "height" "20px"
        , style "left" xPos
        , style "top" yPos
        , style "text-align" "center"
        , style "color" "blue"
        , style "font-size" "10px"
        ]
        [ text textSection.title
        , br [] []
        , text textSection.description
        ]

viewSegment : Point -> Html Msg
viewSegment point =
   div
       [ style "position" "absolute"
       , style "width" "20px"
       , style "height" "20px"
       , style "background-color" "green"
       , style "left" (String.fromInt (point.x * 20) ++ "px")
       , style "top" (String.fromInt (point.y * 20) ++ "px")
       ]
       []

main : Program () Model Msg
main =
   Browser.element
       { init = \_ -> ( init, Cmd.none )
       , view = view
       , update = update
       , subscriptions = subscriptions
       }
