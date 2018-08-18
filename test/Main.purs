module Test.Main where

import Prelude

import ChocoPie (runChocoPie)
import Effect (Effect)
import Effect.Console (logShow)
import FRP.Event (Event, subscribe)
import Test.Assert (assert)

main :: Effect Unit
main = do
  logShow "This program will run and print 1"
  program

  where
    program = do
      let
        drivers =
          { a: \_ -> pure $ pure 1 :: Event Int
          , b: \events ->
              void $ subscribe events \n -> do
                assert $ n == 1
                logShow n
          }
      runChocoPie main' drivers
    main' sources =
      { a: mempty :: Event Unit
      , b: sources.a
      }
