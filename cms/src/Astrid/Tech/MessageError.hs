module Astrid.Tech.MessageError where

-- | Represents a type that may be successful or fail, while at the same time
-- | outputting errors as it goes.
data MessageError m e a = Err [m] e | Ok [m] a

-- | Gets all the messages outputted. The messages will be in reverse order.
messages :: MessageError m e a -> [m]
messages (Ok ms _) = ms
messages (Err ms _) = ms

-- | Converts into an Either object. Note that this will erase all messages accumulated.
toEither :: MessageError m e a -> Either e a
toEither (Ok _ x) = Right x
toEither (Err _ e) = Left e

fromEither :: Either e a -> MessageError m e a 
fromEither (Right x) = Ok [] x
fromEither (Left x) = Err [] x 


instance Functor (MessageError m e) where
  fmap f (Ok ms x) = Ok ms (f x)
  fmap _ (Err ms x) = Err ms x

instance Monad (MessageError m e) where
  return = Ok []

  Err ms e >>= _ = Err ms e
  Ok ms x >>= f =
    case f x of
      Err ms' e -> Err (ms' ++ ms) e
      Ok ms' x' -> Ok (ms' ++ ms) x'

instance Applicative (MessageError m e) where
  pure = return
  a <*> b = do x <- a; y <- b; return (x y)
