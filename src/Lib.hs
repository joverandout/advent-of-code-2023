module Lib
  ( seperated,
  )
where

import Control.Applicative

seperated :: Alternative f => f a -> f b -> f [a]
seperated p filler = (:) <$> p <*> many (filler *> p)