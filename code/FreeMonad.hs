{-# LANGUAGE ScopedTypeVariables #-}

module FreeMonad where

-- doubleBubble :: forall f1 f2 a b
--               . ( Applicative f1
--                 , Applicative f2 )
--              => f1 (f2 (a -> b))
--              -> f1 (f2 a)
--              -> f1 (f2 b)
-- doubleBubble f ffa =
--   let x :: z
--       x = fmap (<*>) f
--   in undefined

doubleBubble :: forall f1 f2 a b
              . ( Applicative f1
                , Applicative f2 )
             => f1 (f2 (a -> b))
             -> f1 (f2 a)
             -> f1 (f2 b) -- <---
doubleBubble f ffa =      ------ hmm
  let x :: f1 (f2 b) -- <--------
      x = (fmap (<*>) f) <*> ffa
  in x

-- let liftApply :: f (g (a -> b))
--               -> f (g a -> g b)
--     liftApply func = (<*>) <$> func

--     apF :: f (g a -> g b)
--     apF = liftApply f

--     apApF :: f (g a) -> f (g b)
--     apApF = (<*>) apF
-- in Compose (apApF x)

data Free f a
  = Pure a
  | Free (f (Free f a))

-- These are just to shut the compiler up, we
-- are not concerned with these right now.
instance Functor f => Functor (Free f) where
  fmap = undefined

instance Functor f => Applicative (Free f) where
  pure = undefined
  (<*>) = undefined
-- Okay, we do care about the Monad though.

-- instance Functor f => Monad (Free f) where
--   return a     = Pure a
--   Pure a >>= f = f a
--   Free f >>= g =
--     let x :: Void
--         x = undefined
--     in Free x

-- instance Functor f => Monad (Free f) where
--   return a     = Pure a
--   Pure a >>= f = f a
--   Free f >>= g = Free _a

-- pleaseShow :: Show a => Bool -> a -> Maybe String
-- pleaseShow False _ = Nothing
-- pleaseShow True a = Just (show _a)

-- pleaseShow :: Show a => Bool -> a -> Maybe String
-- pleaseShow False _ = Nothing
-- pleaseShow True a =
--   let x :: z
--       x = a
--   in Just (show undefined)

-- data V

-- pleaseShow :: Show a => Bool -> a -> Maybe String
-- pleaseShow False _ = Nothing
-- pleaseShow True a =
--   let x :: V
--       x = undefined
--   in Just (show x)

-- f :: (b -> a) -> (c -> b) -> c -> a
-- f = 

-- quinsanity :: ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity f fffffa =
--   undefined

-- quinsanity :: ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: x
--       a = (<*>) <$> func
--   in undefined

-- quinsanity :: ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: f1 (f2 (f3 (f4 (f5 (a -> b)))))
--       a = func
--   in undefined

-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: f1 (f2 (f3 (f4 (f5 (a -> b)))))
--       a = func
--   in undefined


-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a =
--         (fmap . fmap . fmap) (<*>)
--         ((fmap . fmap . fmap . fmap) (<*>) func)
--   in undefined

-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a =
--         (fmap . fmap) (<*>)
--          ((fmap . fmap . fmap) (<*>)
--           ((fmap . fmap . fmap . fmap) (<*>) func))
--   in undefined


-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a =
--         (fmap) (<*>)
--         ((fmap . fmap) (<*>)
--          ((fmap . fmap . fmap) (<*>)
--           ((fmap . fmap . fmap . fmap) (<*>) func)))
--   in undefined

-- • Couldn't match expected type ‘z’
--                   with actual type ‘f1 (f2 (f3 (f4 (f5 (a -> b)))))’

-- doubleBubble :: ( Applicative f1
--                 , Applicative f2 )
--              => f1 (f2 (a -> b))
--              -> f1 (f2 a)
--              -> f1 (f2 b)
-- doubleBubble f ffa =
--   undefined


-- doubleBubble :: ( Applicative f1
--                 , Applicative f2 )
--              => f1 (f2 (a -> b))
--              -> f1 (f2 a)
--              -> f1 (f2 b)
-- doubleBubble f ffa =
--   let x :: z
--       x = f
--   in undefined

-- doubleBubble :: ( Applicative f1
--                 , Applicative f2 )
--              => f1 (f2 (a -> b))
--              -> f1 (f2 a)
--              -> f1 (f2 b)
-- doubleBubble f ffa =
--   let x :: f1 (f2 (a -> b))
--       x = f
--   in undefined

-- ```haskell
-- quinsanity :: ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity f fffffa =
--   undefined
-- ```

-- ```haskell
-- quinsanity :: ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a = func
--   in undefined
-- ```

-- ```haskell
-- quinsanity :: ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: f1 (f2 (f3 (f4 (f5 (a -> b)))))
--       a = func
--   in undefined
-- ```

-- ```haskell
-- {-# LANGUAGE ScopedTypeVariables #-}

-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: f1 (f2 (f3 (f4 (f5 (a -> b)))))
--       a = func
--   in undefined
-- ```

-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a =
--         ((fmap . fmap . fmap) (<*>)
--          ((fmap . fmap . fmap . fmap) (<*>) func)) <*> fffffa
--   in undefined

-- ```haskell
-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a = (<*>) func
--   in undefined
-- ```

-- ```
--       Expected type: f1 (a0 -> f3 (f4 (f5 (a -> b))))
-- ```

-- ```haskell
-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a = (fmap) (<*>) func
--   in undefined
-- ```

-- ```
--       Expected type: f1 (f2 (a0 -> f4 (f5 (a -> b))))
-- ```

-- ```haskell
-- a = (fmap . fmap) (<*>) func
-- ```

-- ```
--       Expected type: f1 (f2 (f3 (a0 -> f5 (a -> b))))
-- ```

-- ```haskell
-- a = (fmap . fmap . fmap) (<*>) func
-- ```

-- ```
--       Expected type: f1 (f2 (f3 (f4 (a0 -> a -> b))))
-- ```

-- ```haskell
-- a = (fmap . fmap . fmap . fmap) (<*>) func
-- ```

-- ```
--       f1 (f2 (f3 (f4 (f5 a -> f5 b))))
-- ```

-- ```haskell
-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a =
--         (fmap . fmap . fmap) (<*>)
--         ((fmap . fmap . fmap . fmap) (<*>) func)
--   in undefined
-- ```

-- ```
-- f1 (f2 (f3 (f4 (f5 a) -> f4 (f5 b))))
-- ```

-- ```haskell
-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a =
--         (fmap . fmap) (<*>)
--          ((fmap . fmap . fmap) (<*>)
--           ((fmap . fmap . fmap . fmap) (<*>) func))
--   in undefined
-- ```

-- ```
-- f1 (f2 (f3 (f4 (f5 a)) -> f3 (f4 (f5 b))))
-- ```

-- ```haskell
-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a =
--         (fmap) (<*>)
--         ((fmap . fmap) (<*>)
--          ((fmap . fmap . fmap) (<*>)
--           ((fmap . fmap . fmap . fmap) (<*>) func)))
--   in undefined
-- ```

-- ```
-- f1 (f2 (f3 (f4 (f5 a))) -> f2 (f3 (f4 (f5 b))))
-- ```

-- We seem to have a function that does what we want now. Lets assert the goal type and see if it passes.

-- ```haskell
-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: f1 (f2 (f3 (f4 (f5 a))))
--         -> f1 (f2 (f3 (f4 (f5 b))))
--       a =
--         (fmap) (<*>)
--         ((fmap . fmap) (<*>)
--          ((fmap . fmap . fmap) (<*>)
--           ((fmap . fmap . fmap . fmap) (<*>) func)))
--   in undefined
-- ```


-- quinsanity :: forall f1 f2 f3 f4 f5 a b
--             . ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a =
--         (fmap (<*>)
--          ((fmap . fmap) (<*>)
--           ((fmap . fmap . fmap) (<*>)
--            ((fmap . fmap . fmap . fmap) (<*>) func)))) <*> fffffa
--   in undefined

      -- Expected type: f1 (a0 -> f3 (f4 (f5 (a -> b))))
      --   Actual type: f1 (f2 (f3 (f4 (f5 (a -> b)))))

    -- • Couldn't match expected type ‘z’
    --               with actual type ‘f1 a0 -> f1 (f3 (f4 (f5 (a -> b))))’

     -- Expected type: f1 (f2 (a0 -> f4 (f5 (a -> b))))
     --    Actual type: f1 (f2 (f3 (f4 (f5 (a -> b)))))

-- quinsanity :: ( Applicative f1
--               , Applicative f2
--               , Applicative f3
--               , Applicative f4
--               , Applicative f5 )
--            => f1 (f2 (f3 (f4 (f5 (a -> b)))))
--            -> f1 (f2 (f3 (f4 (f5 a))))
--            -> f1 (f2 (f3 (f4 (f5 b))))
-- quinsanity func fffffa =
--   let a :: z
--       a = ((<*>) <$>
--           ((fmap . fmap) (<*>)
--           ((fmap . fmap . fmap) (<*>)
--           ((fmap . fmap . fmap . fmap) (<*>) func))))
--   in undefined
