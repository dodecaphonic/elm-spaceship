module Spaceship where


import Window
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


playerImage =
  image 50 52 "sprites/player.png"

gameStage : (Int, Int) -> Element
gameStage (w, h) =
  layers [
    collage w h
      [ rect (toFloat w) (toFloat h)
          |> filled lightBlue
      , playerImage
          |> toForm
          |> move (((toFloat w) / 2.0) - 25.0) ((toFloat h) - 26.0)
      ]
  ]


main : Signal Element
main =
  Signal.map gameStage Window.dimensions
