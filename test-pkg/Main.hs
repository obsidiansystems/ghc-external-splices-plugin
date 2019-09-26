{-# LANGUAGE TemplateHaskell #-}
module Main where

data Foo = A Int | B Char

[d| instance Show Foo where
      show (A i) = show i
      show (B c) = show c   |]

main :: IO ()
main = print $([|A 1|])

