module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = if (all (`elem` ['C','G','A','T']) xs)
           then Right trans
           else Left $ head [x | x <- xs, not $ x `elem` ['C','G','A','T']]
           where trans = (map convertNucleotide xs)

convertNucleotide :: Char -> Char
convertNucleotide x
    | x == 'G' = 'C'
    | x == 'C' = 'G'
    | x == 'T' = 'A'
    | x == 'A' = 'U'
    | otherwise = x


{- toRNA :: String -> Either Char String
toRNA = traverse fromDNA
  where
    fromDNA :: Char -> Either Char Char
    fromDNA 'G' = pure 'C'
    fromDNA 'C' = pure 'G'
    fromDNA 'T' = pure 'A'
    fromDNA 'A' = pure 'U'
    fromDNA c = Left c -}

{- toRNA :: String -> Either Char String
toRNA = traverse convert
convert :: Char -> Either Char Char
convert c = case c of
        'G' -> Right 'C'
        'C' -> Right 'G'
        'T' -> Right 'A'
        'A' -> Right 'U'
        otherwise -> Left c -}
