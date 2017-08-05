module Test.Main where

import Prelude

import ChocoPie (runChocoPie)
import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Timer (TIMER)
import Data.Monoid (mempty)
import FRP (FRP)
import FRP.Event (Event, subscribe)
import Node.Process (PROCESS)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

type MyEffects e =
  ( console :: CONSOLE
  , timer :: TIMER
  , avar :: AVAR
  , process :: PROCESS
  , frp :: FRP
  | e
  )

main :: forall e.
  Eff
    (MyEffects e)
    Unit
main = run [consoleReporter] do
  describe "purescript-choco-pie" do
    it "compiles a test example and returns correct value" do
      result <- program
      result `shouldEqual` 1

  where
    program =
      makeAff \e s -> do
      let
        drivers ::
          { a :: Event Unit -> Eff (MyEffects e) (Event Int)
          , b :: Event Int -> Eff (MyEffects e) Unit
          }
        drivers =
          { a: const $ pure $ pure 1
          , b: \events ->
              subscribe events \n -> do
                logShow n
                s n -- easy way out to make program terminate
          }
      runChocoPie main' drivers
    main' ::
      { a :: Event Int
      , b :: Unit
      } ->
      { a :: Event Unit
      , b :: Event Int
      }
    main' sources =
      { a: mempty
      , b: sources.a
      }

program' :: forall e.
  Eff
    ( frp :: FRP
    , console :: CONSOLE
    | e
    )
    Unit
program' = runChocoPie main' drivers
  where
    main' ::
      { a :: Event Int
      , b :: Unit
      } ->
      { a :: Event Unit
      , b :: Event Int
      }
    main' sources =
      { a: mempty
      , b: sources.a
      }

    drivers ::
      { a :: Event Unit
          -> Eff
               ( frp :: FRP
                , console :: CONSOLE
                | e
                )
               (Event Int)
      , b :: Event Int
          -> Eff
                ( frp :: FRP
                , console :: CONSOLE
                | e
                )
               Unit
      }
    drivers =
      { a: const $ pure (pure 1)
      , b: \events -> subscribe events logShow
      }