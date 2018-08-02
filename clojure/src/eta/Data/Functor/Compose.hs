{-# LANGUAGE DeriveFunctor #-}
module Data.Functor.Compose where


newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Functor)


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  Compose f <*> Compose v = Compose $ (<*>) <$> f <*> v
