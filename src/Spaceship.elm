module Spaceship where


import Window
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (fps)
import Keyboard


type alias WindowHeight = Float


type alias Shooting = Bool


type alias Shot = { x: Float
                  , y: Float
                  }


type alias Model = { x : Float
                   , y : Float
                   , shots : List Shot
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


laserVelocity : Float
laserVelocity = 10.0


spaceshipImage : Element
spaceshipImage =
  let
    (w, h) = spaceshipDimensions
  in
    image w h "sprites/player.png"


spaceship : Model
spaceship = { x = 0
            , y = 0
            , shots = []
            }


update : (Float, Keys, Shooting, WindowHeight) -> Model -> Model
update (dt, arrows, shooting, wh) spaceship =
  spaceship
    |> glide dt arrows wh
    |> shoot shooting wh
    |> updateShots wh


glide : Float -> Keys -> WindowHeight -> Model -> Model
glide dt arrows wh spaceship =
  let
    (_, sh) =
      spaceshipDimensions

    spaceshipY =
      (toFloat sh) - wh / 2
  in
    { spaceship | x <- spaceship.x + dt * (toFloat arrows.x)
                , y <- spaceshipY
    }


shoot : Shooting -> WindowHeight -> Model -> Model
shoot shooting wh spaceship =
  let
    (_, lh) =
      laserDimensions

    laserY =
      (toFloat lh) + spaceship.y
  in
    if shooting then
      { spaceship | shots <- { x = spaceship.x, y = laserY } :: spaceship.shots }
    else
      spaceship


updateShots : WindowHeight -> Model -> Model
updateShots wh spaceship =
  let
    updateShot s = { s | y <- s.y + laserVelocity }
    updated      = List.map updateShot spaceship.shots
    inScreen     = List.filter (\s -> s.y < (wh / 2)) updated
  in
    { spaceship | shots <- inScreen }


renderShot : Float -> Float -> Shot -> Form
renderShot w h shot =
  laserImage
    |> toForm
    |> move (shot.x, shot.y)


gameStage : (Int, Int) -> Model -> Element
gameStage (w', h') spaceship =
  let
    (w, h) =
      (toFloat w', toFloat h')

    (_, sh) =
      spaceshipDimensions

    (_, lh) =
      laserDimensions

    laserY =
      (toFloat lh) - h / 2 + 10

    spaceshipPosition =
      (spaceship.x, spaceship.y)

    laserPosition =
      (spaceship.x, laserY)
  in
    layers [
      collage w' h'
        ([ rect w h
            |> filled black
        , laserImage
          |> toForm
            |> move laserPosition
        , spaceshipImage
            |> toForm
            |> move spaceshipPosition
        ] ++ (List.map (renderShot w h) spaceship.shots))
    ]


input : Signal (Float, Keys, Shooting, WindowHeight)
input =
  let
    delta = Signal.map (\t -> t / 5) (fps 30)
    asTuple = (\dt a s h -> (dt, a, s, (toFloat h)))
    inputs = Signal.map4 asTuple delta Keyboard.arrows Keyboard.space Window.height
  in
    Signal.sampleOn delta inputs


main : Signal Element
main =
  Signal.map2 gameStage Window.dimensions (Signal.foldp update spaceship input)
