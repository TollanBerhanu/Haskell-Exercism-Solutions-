module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = foldl1 (\acc x -> if p x then acc ++ x else acc) xs

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = error "You need to implement this function."
