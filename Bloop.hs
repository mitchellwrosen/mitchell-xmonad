module Bloop where

import XMonad

newtype Bloop
  = Bloop Char

instance Message Bloop
