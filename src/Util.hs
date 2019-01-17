module Util where

nextIndex :: [a] -> Maybe Int -> Maybe Int
nextIndex []   _        = Nothing
nextIndex _    Nothing  = Just 0
nextIndex list (Just i) = if i + 1 == length list then Just i else Just (i + 1)

prevIndex :: [a] -> Maybe Int -> Maybe Int
prevIndex []   _        = Nothing
prevIndex _    Nothing  = Just 0
prevIndex list (Just i) = if i - 1 < 0 then Just 0 else Just (i - 1)
