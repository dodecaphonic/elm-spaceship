module Spaceship where


import Window
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (fps)
import Keyboard
import Random exposing (initialSeed, generate, float, Seed)


type alias Height = Float


type alias Width = Float


type alias Shooting = Bool


type alias Shot = { x: Float
                  , y: Float
                  }


type alias Bullet = { x: Float
                    , y: Float
                    }


type alias Model = { spaceshipX : Float
                   , spaceshipY : Float
                   , shots : List Shot
                   , bullets : List Bullet
                   , bulletSeed : Seed
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


bulletImage : Element
bulletImage =
  image 15 35 "sprites/bullet.png"


laserVelocity : Float
laserVelocity = 10.0


spaceshipImage : Element
spaceshipImage =
  let
    (w, h) = spaceshipDimensions
  in
    image w h "sprites/player.png"


game : Model
game = { spaceshipX = 0
       , spaceshipY = 0
       , shots = []
       , bullets = []
       , bulletSeed = initialSeed 1337
       }


update : (Float, Keys, Shooting, (Width, Height)) -> Model -> Model
update (dt, arrows, shooting, (ww, wh)) game =
  game
    |> glide dt arrows wh
    |> shoot shooting wh
    |> updateShots wh
    |> maybeAddBullet ww wh
    |> updateBullets dt wh


glide : Float -> Keys -> Height -> Model -> Model
glide dt arrows wh game =
  let
    (_, sh) =
      spaceshipDimensions

    spaceshipY =
      (toFloat sh) - wh / 2
  in
    { game | spaceshipX <- game.spaceshipX + dt * (toFloat arrows.x)
           , spaceshipY <- spaceshipY
    }


shoot : Shooting -> Height -> Model -> Model
shoot shooting wh game =
  let
    (_, lh) =
      laserDimensions

    laserY =
      (toFloat lh) + game.spaceshipY
  in
    if shooting then
      { game | shots <- { x = game.spaceshipX, y = laserY } :: game.shots }
    else
      game


updateShots : Height -> Model -> Model
updateShots wh game =
  let
    updateShot s = { s | y <- s.y + laserVelocity }
    updated      = List.map updateShot game.shots
    inScreen     = List.filter (\s -> s.y < (wh / 2)) updated
  in
    { game | shots <- inScreen }


renderShot : Width -> Height -> Shot -> Form
renderShot w h shot =
  laserImage
    |> toForm
    |> move (shot.x, shot.y)


maybeAddBullet : Width -> Height -> Model -> Model
maybeAddBullet ww wh game =
  let
    (probability, seed') =
      generate (float 0 1) game.bulletSeed
  in
    if probability < 0.9 then
      { game | bulletSeed <- seed' }
    else
      let
        (position, seed'') =
          generate (float -(ww / 2) (ww / 2)) seed'
      in
        { game | bullets <- { x = position, y = (wh / 2) } :: game.bullets
               , bulletSeed <- seed''
        }


gravity : Float
gravity = 10.0


updateBullets : Float -> Height -> Model -> Model
updateBullets dt wh game =
  let
    updateBullet b = { b | y <- b.y - ((dt / 8) * gravity) }
    updated = List.map updateBullet game.bullets
    inScreen = updated -- List.filter (\b -> b.y > (wh / 2)) updated
  in
    { game | bullets <- inScreen }


renderBullet : Width -> Height -> Bullet -> Form
renderBullet w h bullet =
  bulletImage
    |> toForm
    |> move (bullet.x, bullet.y)


gameStage : (Int, Int) -> Model -> Element
gameStage (w', h') game =
  let
    (w, h) =
      (toFloat w', toFloat h')

    (_, lh) =
      laserDimensions

    laserY =
      (toFloat lh) - h / 2 + 10

    spaceshipPosition =
      (game.spaceshipX, game.spaceshipY)

    laserPosition =
      (game.spaceshipX, laserY)
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
        ] ++ (List.map (renderShot w h) game.shots)
          ++ (List.map (renderBullet w h) game.bullets))
    ]


input : Signal (Float, Keys, Shooting, (Width, Height))
input =
  let
    delta = Signal.map (\t -> t / 5) (fps 30)
    asTuple = (\dt a s (w, h) -> (dt, a, s, (toFloat w, toFloat h)))
    inputs = Signal.map4 asTuple delta Keyboard.arrows Keyboard.space Window.dimensions
  in
    Signal.sampleOn delta inputs


main : Signal Element
main =
  Signal.map2 gameStage Window.dimensions (Signal.foldp update game input)
