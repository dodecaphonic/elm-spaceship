module Spaceship where


import Window
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (fps)
import Keyboard


type alias Model = { x : Float
                   , y : Float
                   }


type alias Keys = { x : Int, y : Int }


spaceshipDimensions : (Int, Int)
spaceshipDimensions = (50, 52)


laserDimensions : (Int, Int)
laserDimensions = (7, 33)


laserImage : Element
laserImage =
  let
    (w, h) = laserDimensions
  in
    image w h "sprites/laser.png"


spaceshipImage : Element
spaceshipImage =
  let
    (w, h) = spaceshipDimensions
  in
    image w h "sprites/player.png"


glide : Float -> Keys -> Model -> Model
glide dt keys spaceship =
  { spaceship | x <- spaceship.x + dt * (toFloat keys.x) }


spaceship : Model
spaceship = { x = 0
            , y = 0
            }


update : (Float, Keys) -> Model -> Model
update (dt, keys) spaceship =
  spaceship
    |> glide dt keys


gameStage : (Int, Int) -> Model -> Element
gameStage (w', h') spaceship =
  let
    (w, h) =
      (toFloat w', toFloat h')

    (sw, sh) =
      spaceshipDimensions

    (_, lh) =
      laserDimensions

    spaceshipY =
      (toFloat sh) - h / 2

    laserY =
      (toFloat lh) - h / 2 + (toFloat sh) + 10

    spaceshipPosition =
      (spaceship.x, spaceshipY)

    laserPosition =
      (spaceship.x, laserY)
  in
    layers [
      collage w' h'
        [ rect w h
            |> filled black
        , spaceshipImage
            |> toForm
            |> move spaceshipPosition
        , laserImage
            |> toForm
            |> move laserPosition
        ]
    ]


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t / 5) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)


main : Signal Element
main =
  Signal.map2 gameStage Window.dimensions (Signal.foldp update spaceship input)
