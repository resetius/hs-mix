{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Mix.Types where

newtype Byte = Byte Int deriving (Eq, Show)

data Register = Register {
    sign :: Bool,
    a1 :: Byte,
    a2 :: Byte,
    a3 :: Byte,
    a4 :: Byte,
    a5 :: Byte
} deriving (Eq, Show)

data ShortRegister = ShortRegister {
    sgn :: Bool,
    x1 :: Byte,
    x2 :: Byte
} deriving (Eq, Show)

instance Num Byte where
    Byte a + Byte b = Byte ((a + b) `mod` 64)
    Byte a * Byte b = Byte ((a * b) `mod` 64)
    Byte a - Byte b = Byte ((64 + a - b) `mod` 64)
    abs x = x
    signum a = case a of
        Byte 0 -> 0
        Byte _ -> 1

    fromInteger a = Byte (fromInteger $ abs a `mod` 64)

instance Bounded Byte where
    minBound = 0
    maxBound  = 64

data State = State {
    a:: Register,
    x:: Register,
    i1:: ShortRegister,
    i2:: ShortRegister,
    i3:: ShortRegister,
    i4:: ShortRegister,
    i5:: ShortRegister,
    i6:: ShortRegister,
    j:: ShortRegister
} deriving (Eq, Show)

data Range = Empty | Int Int
