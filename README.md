# Purescript-ChocoPie

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
