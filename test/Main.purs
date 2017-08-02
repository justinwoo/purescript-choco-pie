module Test.Main where

import Prelude

import ChocoPie (chocoPieItUp, runChocoPie)
import Control.Monad.Aff (delay, launchAff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Monoid (mempty)
import Data.Newtype (wrap)
import FRP (FRP)
import FRP.Event (Event, subscribe)
import FRP.Event.Time (interval)
import Global.Unsafe (unsafeStringify)
import Unsafe.Coerce (unsafeCoerce)
  
main = void $ makeAff \e s -> do
  let
    drivers ::
      { a :: Event Unit -> Eff _ (Event Int)
      , b :: Event Int -> Eff _ Unit
      }
    drivers =
      { a: const $ pure $ pure 1
      , b: \events ->
          subscribe events \n -> do
            logShow n
            unsafeCoerce $ e -- easy way out to make program terminate
      }
  _ <- runChocoPie main' drivers
  log "Should have printed 1."
  pure unit
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

program = runChocoPie main' drivers
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
      { a :: Event Unit -> Eff _ (Event Int)
      , b :: Event Int -> Eff _ Unit
      }
    drivers =
      { a: const $ pure (pure 1)
      , b: \events -> subscribe events logShow
      }