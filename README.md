# Purescript-ChocoPie

[![Build Status](https://travis-ci.org/justinwoo/purescript-choco-pie.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-choco-pie)

A Cycle.js-like utility for working with Purescript-Behaviors Events. Aptly named for a circular Korean snack food.

![](http://i.imgur.com/uNCB4qp.jpg)

## Usage

```purs
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
```

With appropriate context and individual annotations, the type signature annotations are no longer needed.

## Usage Exmples

I rewrote some code in my [simple-rpc-telegram-bot](https://github.com/justinwoo/simple-rpc-telegram-bot/blob/7ebdce679eba0eb4462d14d3a6e51d1ba245aa6f/src/Main.purs#L126) repo, where I have drivers for running a child process, receiving and sending messages from Telegram, and a timer that ticks for every hour.

```purs

type Main
   = { torscraper :: Event Result
     , bot :: Event Request
     , timer :: Event Request
     }
  -> { torscraper :: Event Request
     , bot :: Event Result
     , timer :: Event Unit
     }
main' :: Main
main' sources =
  { torscraper: sources.timer <|> sources.bot
  , bot: sources.torscraper
  , timer: mempty
  }
 
 type Drivers e =
  { torscraper :: Event Request -> Eff e (Event Result)
  , bot :: Event Result -> Eff e (Event Request)
  , timer :: Event Unit -> Eff e (Event Request)
  }

drivers :: forall e. Config -> Drivers (infinity :: INFINITY | e)
drivers
  { token
  , torscraperPath
  , master
  } =
  { torscraper
  , bot
  , timer
  }
  where
    torscraper requests = -- ...
    
    bot results = -- ...

    timer _
      | tick <- pure 0 <|> interval (60 * 60 * 1000)
      , reqs <- { origin: FromTimer, id: master } <$ tick
      = pure reqs

-- runChocoPie main' (drivers config)
```
