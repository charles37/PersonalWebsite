module Main exposing (..)
import List.Extra

import Browser
import Task
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
import Browser.Dom
import Browser.Navigation
import Random 


type alias Model =
    { snake : Snake
    , boxWidth : Int
    , boxHeight : Int
    , textSections : List TextSection
    , food : Food
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

type alias Food =
    { position : Point
    , iconClass : String
    }


type alias Point =
    { x : Int
    , y : Int
    }

type alias TextSection =
    { title : String
    , position : Point
    , url : String
    , iconClass : String
    }

initTextSections : List TextSection
initTextSections =
    [ { title = "Resume"
      , position = { x = 0, y = 0 }
      , url = "https://your_resume_url_here"
      , iconClass = "fas fa-file"
      }
    , { title = "GitHub"
      , position = { x = 19, y = 0 }
      , url = "https://github.com/charles37"
      , iconClass = "fab fa-github"
      }
    , { title = "Twitter"
      , position = { x = 0, y = 19 }
      , url = "https://twitter.com/benprevor"
      , iconClass = "fab fa-twitter"
      }
    , { title = "Blog"
      , position = { x = 19, y = 19 }
      , url = "https://substack.com"
      , iconClass = "fas fa-blog"
      }
    ]

updateTextSectionPositions : Int -> Int -> List TextSection -> List TextSection
updateTextSectionPositions boxWidth boxHeight myTextSections =
    List.map
        (\textSection ->
            let
                xPos = if textSection.position.x == 0 then 0 else (boxWidth - 1)
                yPos = if textSection.position.y == 0 then 0 else (boxHeight - 1)
            in
            { textSection | position = { x = xPos, y = yPos } }
        )
        myTextSections

updateFoodPosition : Int -> Int -> Food -> Food
updateFoodPosition boxWidth boxHeight food =
    let
        -- x and y should start at center of box
        xPos = boxWidth // 2
        yPos = boxHeight // 2
    in
    { food | position = { x = xPos, y = yPos } }
       

 

init : ( Model, Cmd Msg )
init =
    let
        boxWidth = 20
        boxHeight = 20
        centerX = boxWidth // 2
        centerY = boxHeight // 2
        model =
            { snake =
                { body =
                    [ { x = centerX, y = centerY }
                    , { x = centerX - 1, y = centerY }
                    , { x = centerX - 2, y = centerY }
                    ]
                , direction = Right
                }
            , boxWidth = boxWidth
            , boxHeight = boxHeight
            , textSections = initTextSections
            , food =
                { position = { x = centerX - 1, y = centerY - 1 }
                , iconClass = "fas fa-cookie-bite"
                }
            }
    in
    ( model
        , Task.perform
            (\viewport ->
                let
                    ( bw, bh ) =
                        windowSizeToBoxDimensions (round viewport.viewport.width) (round viewport.viewport.height)
                    updatedTextSections = updateTextSectionPositions bw bh initTextSections
                    updatedFood = updateFoodPosition bw bh model.food
                in
                ResizeWindow bw bh updatedTextSections updatedFood
            )
            Browser.Dom.getViewport
        )


type Msg
    = ChangeDirection Direction
    | Move
    | ResizeWindow Int Int (List TextSection) Food
    | Navigate String
    | NewFood Food

wrapMessage : msg -> Cmd msg
wrapMessage msg =
    Task.succeed msg
        |> Task.perform identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDirection newDirection ->
            ( over snake_ (assign direction_ newDirection) model , Cmd.none )
            |> Debug.log "newDirection"
        Move ->
            let
                newHead = getNewHead model.snake
                collidedSection = collisionDetected newHead model.textSections 
                collidedFood = newHead == model.food.position 
            in
            case (collidedSection,collidedFood) of
                (Just section, _) ->
                    ( model, wrapMessage (Navigate section.url) )
                (_, True) ->
                        let
                            largeNewSnake = assign body_ (newHead :: model.snake.body) model.snake 
                        in
                            ( { model | snake = largeNewSnake } 
                            , randomFood model
                            )
                _ -> ( { model | snake = moveSnake model.snake }, Cmd.none )
        NewFood newFood ->
            ( { model | food = newFood }, Cmd.none )
        ResizeWindow width height updatedTextSections updatedFood ->
            ( { model | boxWidth = width, boxHeight = height, textSections = updatedTextSections, food = updatedFood }, Cmd.none )
        Navigate url ->
            ( model, Browser.Navigation.load url )

randomFood : Model -> Cmd Msg
randomFood model =
    let
        randomXY =
            Random.pair (Random.int 0 (model.boxWidth - 1)) (Random.int 0 (model.boxHeight - 1))
    in
    Random.generate 
        (\( x, y ) ->
            let
                newFood =
                    { position = { x = x, y = y }, iconClass = "fas fa-cookie-bite" }
            in
            NewFood newFood
        )
        randomXY
    
getNewHead : Snake -> Point
getNewHead snake =
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
    newHead

moveSnake : Snake -> Snake
moveSnake snake =
    assign body_ (getNewHead snake :: (withDefault [] (List.Extra.init snake.body))) snake

collisionDetected : Point -> List TextSection -> Maybe TextSection
collisionDetected point textParts=
    List.filter (\textSection -> textSection.position == point) textParts 
    |> List.head

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
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown
            (Json.Decode.map ChangeDirection directionDecoder)
        , Time.every 200 (\_ -> Move)
        , Browser.Events.onResize (\w h ->
            let
                ( bw, bh ) = windowSizeToBoxDimensions w h
                updatedTextSections = updateTextSectionPositions bw bh model.textSections
                updatedFood = updateFoodPosition bw bh model.food
            in
            ResizeWindow bw bh updatedTextSections updatedFood
          )
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

                        87 -> -- W key
                            Json.Decode.succeed Up

                        83 -> -- S key
                            Json.Decode.succeed Down

                        65 -> -- A key
                            Json.Decode.succeed Left

                        68 -> -- D key
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
            (List.map viewSegment model.snake.body ++ List.map viewTextSection model.textSections ++ [viewFood model.food])
        ]

viewFood : Food -> Html Msg
viewFood model =
    let
        xPos =
            String.fromInt (model.position.x * 20) ++ "px"

        yPos =
            String.fromInt (model.position.y * 20) ++ "px"
    in
    div
        [ style "position" "absolute"
        , style "width" "20px"
        , style "height" "20px"
        , style "left" xPos
        , style "top" yPos
        , style "text-align" "center"
        , style "color" "red"
        , style "font-size" "20px"
        ]
        [ i [ class model.iconClass ] [] ]
    

viewTextSection : TextSection -> Html Msg
viewTextSection textSection =
    let
        xPos =
            if textSection.position.x == 0 then
                "10px"
            else
                "calc(100% - 10px - 30px - 20px)"

        yPos =
            if textSection.position.y == 0 then
                "10px"
            else
                "calc(100% - 10px - 30px - 40px)"
    in
    div
        [ style "position" "absolute"
        , style "width" "30px"
        , style "height" "30px"
        , style "left" xPos
        , style "top" yPos
        , style "text-align" "justify"
        , style "color" "blue"
        , style "font-size" "14px"
        ]
        [ text textSection.title
        , br [] []
        , i [ class textSection.iconClass, style "font-size" "50px" ] [] 
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
       { init = \_ -> init 
       , view = view
       , update = update
       , subscriptions = subscriptions
       }
