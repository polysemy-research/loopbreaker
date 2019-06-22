module Loopbreaker.Utils
  ( (.:)
  ) where


(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)
