--Portik Ábel
--paim1949

data Fa23 a =
    Nodus2 (Fa23 a) a (Fa23 a) |
    Nodus3 (Fa23 a) a (Fa23 a) a (Fa23 a) |
    Level23

----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

instance (Show a) => Show (Fa23 a) where
    show = kiir23 0

----------------------------------------------------------------------------------------------------------------------

kiir23 :: (Show a) => Int -> Fa23 a -> String
kiir23 _ Level23 = ""
kiir23 k (Nodus2 bal csucs jobb) = 
    kiir23 (k+1) bal 
    ++ replicate k '\t' ++ show csucs ++ "\n" 
    ++ kiir23 (k+1) jobb
kiir23 k (Nodus3 bal csucs1 kozep csucs2 jobb) = 
    kiir23 (k+1) bal 
    ++ replicate k '\t' ++ show csucs1 ++ "\n"
    ++ kiir23 (k+1) kozep 
    ++ replicate k '\t' ++ show csucs2 ++ "\n" 
    ++ kiir23 (k+1) jobb

----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

data Beszurt23 a = 
    Egyenlitett (Fa23 a) |
    Feltolt (Fa23 a) a (Fa23 a)

be23 :: (Ord a) => a -> Fa23 a -> Beszurt23 a

be23 uj Level23 = Feltolt Level23 uj Level23
be23 uj (Nodus2 Level23 n Level23)
    | uj < n = Egyenlitett (Nodus3 Level23 uj Level23 n Level23)
    | n < uj = Egyenlitett (Nodus3 Level23 n Level23 uj Level23)
    | otherwise = Egyenlitett (Nodus2 Level23 n Level23)
be23 uj (Nodus3 Level23 n1 Level23 n2 Level23)
    |       uj < n1         = Feltolt (Nodus2 Level23 uj Level23) n1 (Nodus2 Level23 n2 Level23)
    | n1 < uj && uj < n2    = Feltolt (Nodus2 Level23 n1 Level23) uj (Nodus2 Level23 n2 Level23)
    |       n2 < uj         = Feltolt (Nodus2 Level23 n1 Level23) n2 (Nodus2 Level23 uj Level23)
    | otherwise = Egyenlitett(Nodus3 Level23 n1 Level23 n2 Level23)
----------------------------------------------------------------------------------------------------------------------
be23 uj (Nodus2 b n j)
    | uj < n = case be23 uj b of
        Egyenlitett fa -> Egyenlitett (Nodus2 fa n j)
        Feltolt b' n' j' -> Egyenlitett (Nodus3 b' n' j' n j)
    | uj > n = case be23 uj j of 
        Egyenlitett fa -> Egyenlitett (Nodus2 b n fa)
        Feltolt b' n' j' -> Egyenlitett (Nodus3 b n b' n' j')
    | otherwise = Egyenlitett (Nodus2 b n j)
----------------------------------------------------------------------------------------------------------------------
be23 uj (Nodus3 b n1 k n2 j)
    | uj < n1 = case be23 uj b of
        Egyenlitett fa -> Egyenlitett (Nodus3 fa n1 k n2 j) 
        Feltolt b' n' j' -> Feltolt (Nodus2 b' n' j') n1 (Nodus2 k n2 j)
    | n1 < uj && uj < n2 = case be23 uj k of
        Egyenlitett fa -> Egyenlitett (Nodus3 b n1 fa n2 j) 
        Feltolt b' n' j' -> Feltolt (Nodus2 b n1 b') n' (Nodus2 j' n2 j)
    | n2 < uj = case be23 uj j of
        Egyenlitett fa -> Egyenlitett (Nodus3 b n1 k n2 fa) 
        Feltolt b' n' j' -> Feltolt (Nodus2 b n1 k) n2 (Nodus2 b' n' j')
    | otherwise = Egyenlitett (Nodus3 b n1 k n2 j)

----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

beszur23 :: (Ord a) => a -> Fa23 a -> Fa23 a

beszur23 uj fa = case be23 uj fa of
    Egyenlitett fa -> fa
    Feltolt b n j -> Nodus2 b n j

----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

listabol23 :: (Ord a) => [a] -> Fa23 a
listabol23 = foldr beszur23 Level23

----------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

data Torolt23 a =
    Hianyos (Fa23 a) |
    Teljes (Fa23 a)

kivesz23 :: (Ord a) => a -> Fa23 a -> Maybe (Torolt23 a)


----------------------------------------------------------------------------------------------------------------------
-- (n)
kivesz23 t (Nodus2 Level23 n Level23)
    | t == n = Just (Hianyos Level23)
    | otherwise = Nothing 
-- (n1,n2)
kivesz23 t (Nodus3 Level23 n1 Level23 n2 Level23)
    | t == n1 = Just (Teljes (Nodus2 Level23 n2 Level23))
    | t == n2 = Just (Teljes (Nodus2 Level23 n1 Level23))
    | otherwise = Nothing

----------------------------------------------------------------------------------------------------------------------
-- 2_2 ( (b1-n1-j1) n (b2-n2-j2) )
kivesz23 t (Nodus2 (Nodus2 b1 n1 j1) n (Nodus2 b2 n2 j2))
    | t < n = case kivesz23 t (Nodus2 b1 n1 j1) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus2 fa n (Nodus2 b2 n2 j2)))
        (Just (Hianyos fa)) -> Just (Hianyos (Nodus3 fa n b2 n2 j2))
        Nothing -> Nothing
    | n < t = case kivesz23 t (Nodus2 b2 n2 j2) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus2 (Nodus2 b1 n1 j1) n fa))
        (Just (Hianyos fa)) -> Just (Hianyos (Nodus3 b1 n1 j1 n fa))
        Nothing -> Nothing
    | otherwise = case kiveszMax23 (Nodus2 b1 n1 j1) of
        (Just max_b) -> case kivesz23 max_b (Nodus2 b1 n1 j1) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus2 fa max_b (Nodus2 b2 n2 j2)))
            (Just (Hianyos fa)) -> Just (Hianyos (Nodus3 fa max_b b2 n2 j2))

-- 3_2 ( (b1-n11-k1-n21-j1) n (b2-n2-j2) )
kivesz23 t (Nodus2 (Nodus3 b1 n11 k1 n21 j1) n (Nodus2 b2 n2 j2))
    | t < n = case kivesz23 t (Nodus3 b1 n11 k1 n21 j1) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus2 fa n (Nodus2 b2 n2 j2)))
        (Just (Hianyos fa)) -> Just (Hianyos (Nodus3 fa n b2 n2 j2))
        Nothing -> Nothing
    | n < t = case kivesz23 t (Nodus2 b2 n2 j2) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus2 (Nodus3 b1 n11 k1 n21 j1) n fa))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus2 b1 n11 k1) n21 (Nodus2 j1 n fa)))
        Nothing -> Nothing
    | otherwise = case kiveszMax23 (Nodus3 b1 n11 k1 n21 j1) of
        (Just max_b) -> case kivesz23 max_b (Nodus3 b1 n11 k1 n21 j1) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus2 fa max_b (Nodus2 b2 n2 j2)))

-- 2_3 ( (b1-n1-j1) n (b2-n12-k2-n22-j2) )
kivesz23 t (Nodus2 (Nodus2 b1 n1 j1) n (Nodus3 b2 n12 k2 n22 j2))
    | t < n = case kivesz23 t (Nodus2 b1 n1 j1) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus2 fa n (Nodus3 b2 n12 k2 n22 j2)))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus2 fa n b2) n12 (Nodus2 k2 n22 j2)))
        Nothing -> Nothing
    | n < t = case kivesz23 t (Nodus3 b2 n12 k2 n22 j2) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus2 (Nodus2 b1 n1 j1) n fa))
        (Just (Hianyos fa)) -> Just (Hianyos (Nodus3 b1 n1 j1 n fa))
        Nothing -> Nothing
    | otherwise = case kiveszMax23 (Nodus2 b1 n1 j1) of
        (Just max_b) -> case kivesz23 max_b (Nodus2 b1 n1 j1) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus2 fa max_b (Nodus3 b2 n12 k2 n22 j2)))
            (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus2 fa max_b b2) n12 (Nodus2 k2 n22 j2)))

-- 3_3 ( (b1-n11-k1-n21-j1) n (b2-n12-k2-n22-j2) )
kivesz23 t (Nodus2 (Nodus3 b1 n11 k1 n21 j1) n (Nodus3 b2 n12 k2 n22 j2))
    | t < n = case kivesz23 t (Nodus3 b1 n11 k1 n21 j1) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus2 fa n (Nodus3 b2 n12 k2 n22 j2)))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus2 fa n b2) n12 (Nodus2 k2 n22 j2)))
        Nothing -> Nothing
    | n < t = case kivesz23 t (Nodus3 b2 n12 k2 n22 j2) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus2 (Nodus3 b1 n11 k1 n21 j1) n fa))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus2 b1 n11 k1) n21 (Nodus2 j1 n fa)))
        Nothing -> Nothing 
    | otherwise = case kiveszMax23 (Nodus3 b1 n11 k1 n21 j1) of
        (Just max_b) -> case kivesz23 max_b (Nodus3 b1 n11 k1 n21 j1) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus2 fa max_b (Nodus3 b2 n12 k2 n22 j2)))
            (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus2 fa max_b b2) n12 (Nodus2 k2 n22 j2)))

----------------------------------------------------------------------------------------------------------------------
-- 2_2_x ( (b1-n11-j1) n1 (b2-n22-j2) n2 j )
kivesz23 t (Nodus3 (Nodus2 b1 n11 j1) n1 (Nodus2 b2 n22 j2) n2 j)
    | t < n1 = case kivesz23 t (Nodus2 b1 n11 j1) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 fa n1 (Nodus2 b2 n22 j2) n2 j))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus3 fa n1 b2 n22 j2) n2 j))
        Nothing -> Nothing
    | t == n1 = case kiveszMax23 (Nodus2 b1 n11 j1) of
        (Just max_b) -> case kivesz23 max_b (Nodus2 b1 n11 j1) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus3 fa max_b (Nodus2 b2 n22 j2) n2 j))
            (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus3 fa max_b b2 n22 j2) n2 j ))
    | t < n2 = case kivesz23 t (Nodus2 b2 n22 j2) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus2 b1 n11 j1) n1 fa n2 j))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus3 b1 n11 j1 n1 fa) n2 j))
        Nothing -> Nothing 
    | t == n2 = case kiveszMax23 (Nodus2 b2 n22 j2) of
        (Just max_k) -> case kivesz23 max_k (Nodus2 b2 n22 j2) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus2 b1 n11 j1) n1 fa max_k j))
            (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus3 b1 n11 j1 n1 fa) max_k j))
    | otherwise = case kivesz23 t j of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus2 b1 n11 j1) n1 (Nodus2 b2 n22 j2) n2 fa))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus2 b1 n11 j1) n1 (Nodus3 b2 n22 j2 n2 fa)))
        Nothing -> Nothing

-- 2_3_x ( (b1-n11-j1) n1 (b2-n12-k2-n22-j2) n2 j )
kivesz23 t (Nodus3 (Nodus2 b1 n11 j1) n1 (Nodus3 b2 n12 k2 n22 j2) n2 j)
    | t < n1 = case kivesz23 t (Nodus2 b1 n11 j1) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 fa n1 (Nodus3 b2 n12 k2 n22 j2) n2 j))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus3 (Nodus2 fa n1 b2) n12 (Nodus2 k2 n22 j2) n2 j))
        Nothing -> Nothing
    | t == n1 = case kiveszMax23 (Nodus2 b1 n11 j1) of
        (Just max_b) -> case kivesz23 max_b (Nodus2 b1 n11 j1) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus3 fa max_b (Nodus3 b2 n12 k2 n22 j2) n2 j))
            (Just (Hianyos fa)) -> Just (Teljes (Nodus3 (Nodus2 fa max_b b2) n12 (Nodus2 k2 n22 j2) n2 j ))
    | t < n2 = case kivesz23 t (Nodus3 b2 n12 k2 n22 j2) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus2 b1 n11 j1) n1 fa n2 j))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus3 b1 n11 j1 n1 fa) n2 j))
        Nothing -> Nothing 
    | t == n2 = case kiveszMax23 (Nodus3 b2 n12 k2 n22 j2) of
        (Just max_k) -> case kivesz23 max_k (Nodus3 b2 n12 k2 n22 j2) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus2 b1 n11 j1) n1 fa max_k j))
            (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus3 b1 n11 j1 n1 fa) max_k j))
    | otherwise = case kivesz23 t j of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus2 b1 n11 j1) n1 (Nodus3 b2 n12 k2 n22 j2) n2 fa))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus3 (Nodus2 b1 n11 j1) n1 (Nodus2 b2 n11 k2) n22 (Nodus2 j2 n2 fa)))
        Nothing -> Nothing

-- 3_2_x ( (b1-n11-k1-n21-j1) n1 (b2-n22-j2) n2 j )
kivesz23 t (Nodus3 (Nodus3 b1 n11 k1 n21 j1) n1 (Nodus2 b2 n22 j2) n2 j)
    | t < n1 = case kivesz23 t (Nodus3 b1 n11 k1 n21 j1) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 fa n1 (Nodus2 b2 n22 j2) n2 j))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus3 fa n1 b2 n22 j2) n2 j))
        Nothing -> Nothing
    | t == n1 = case kiveszMax23 (Nodus3 b1 n11 k1 n21 j1) of
        (Just max_b) -> case kivesz23 max_b (Nodus3 b1 n11 k1 n21 j1) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus3 fa max_b (Nodus2 b2 n22 j2) n2 j))
            (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus3 fa max_b b2 n22 j2) n2 j))
    | t < n2 = case kivesz23 t (Nodus2 b2 n22 j2) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus3 b1 n11 k1 n21 j1) n1 fa n2 j))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus3 (Nodus2 b1 n11 k1) n21 (Nodus2 j1 n1 fa) n2 j))
        Nothing -> Nothing 
    | t == n2 = case kiveszMax23 (Nodus2 b2 n22 j2) of
        (Just max_k) -> case kivesz23 max_k (Nodus2 b2 n22 j2) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus3 b1 n11 k1 n21 j1) n1 fa max_k j))
            (Just (Hianyos fa)) -> Just (Teljes (Nodus3 (Nodus2 b1 n11 k1) n21 (Nodus2 j1 n1 fa) max_k j))
    | otherwise = case kivesz23 t j of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus3 b1 n11 k1 n21 j1) n1 (Nodus2 b2 n22 j2) n2 fa))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus2 (Nodus3 b1 n11 k1 n21 j1) n1 (Nodus3 b2 n22 j2 n2 fa)))
        Nothing -> Nothing

-- 3_3_x ( (b1-n11-k1-n21-j1) n1 (b2-n12-k2-n22-j2) n2 j )
kivesz23 t (Nodus3 (Nodus3 b1 n11 k1 n21 j1) n1 (Nodus3 b2 n12 k2 n22 j2) n2 j)
    | t < n1 = case kivesz23 t (Nodus3 b1 n11 k1 n21 j1) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 fa n1 (Nodus3 b2 n12 k2 n22 j2) n2 j))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus3 (Nodus2 fa n1 b2) n12 (Nodus2 k2 n22 j2) n2 j))
        Nothing -> Nothing
    | t == n1 = case kiveszMax23 (Nodus3 b1 n11 k1 n21 j1) of
        (Just max_b) -> case kivesz23 max_b (Nodus3 b1 n11 k1 n21 j1) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus3 fa max_b (Nodus3 b2 n12 k2 n22 j2) n2 j))
            (Just (Hianyos fa)) -> Just (Teljes (Nodus3 (Nodus2 fa max_b b2) n12 (Nodus2 k2 n22 j2) n2 j))
    | t < n2 = case kivesz23 t (Nodus3 b2 n12 k2 n22 j2) of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus3 b1 n11 k1 n21 j1) n1 fa n2 j))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus3 (Nodus2 b1 n11 k1) n21 (Nodus2 j1 n1 fa) n2 j))
        Nothing -> Nothing 
    | t == n2 = case kiveszMax23 (Nodus3 b2 n12 k2 n22 j2) of
        (Just max_k) -> case kivesz23 max_k (Nodus3 b2 n12 k2 n22 j2) of
            (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus3 b1 n11 k1 n21 j1) n1 fa max_k j))
            (Just (Hianyos fa)) -> Just (Teljes (Nodus3 (Nodus2 b1 n11 k1) n21 (Nodus2 j1 n1 fa) max_k j))
    | otherwise = case kivesz23 t j of
        (Just (Teljes fa)) -> Just (Teljes (Nodus3 (Nodus3 b1 n11 k1 n21 j1) n1 (Nodus3 b2 n12 k2 n22 j2) n2 fa))
        (Just (Hianyos fa)) -> Just (Teljes (Nodus3 (Nodus3 b1 n11 k1 n21 j1) n1 (Nodus2 b2 n12 k2) n22 (Nodus2 j2 n2 fa)))
        Nothing -> Nothing

----------------------------------------------------------------------------------------------------------------------
kiveszMax23 :: (Ord a) => Fa23 a -> Maybe a
kiveszMax23 Level23 = Nothing
kiveszMax23 (Nodus2 _ n j) = case kiveszMax23 j of
    (Just max) -> Just max
    Nothing -> Just n
kiveszMax23 (Nodus3 _ _ _ n2 j) = case kiveszMax23 j of
    (Just max) -> Just max
    Nothing -> Just n2

----------------------------------------------------------------------------------------------------------------------
torol23 :: (Ord a) => a -> Fa23 a -> Fa23 a
torol23 t fa = case kivesz23 t fa of
    (Just (Teljes torolt)) -> torolt
    (Just (Hianyos torolt)) -> torolt
    Nothing -> fa