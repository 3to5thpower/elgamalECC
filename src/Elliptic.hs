{-# LANGUAGE NamedFieldPuns #-}

module Elliptic (ECPoint(ECPoint, InfinitePoint), EllipticCurve(EllipticCurve), apply, discriminant, scalaMul) where

import Data

-- 楕円を表現するデータ型
data EllipticCurve = EllipticCurve { 
    a :: Integer,
    b :: Integer
} deriving (Eq)

instance Show EllipticCurve where
    show (EllipticCurve a b) = "y^2 = x^3 + " ++ show a ++ "x + " ++ show b

apply :: EllipticCurve -> ECPoint -> GField
apply (EllipticCurve {a, b}) (ECPoint{x, y, curve}) = x * x * x + (fromInteger a) * x + (fromInteger b) - y * y

discriminant :: GField -> GField -> GField
discriminant a b = 4 * a * a * a + 27 * b * b

-- 楕円上の座標を表現
data ECPoint = InfinitePoint | ECPoint {
    x :: GField,
    y :: GField,
    curve:: EllipticCurve
}

instance Show ECPoint where
    show (ECPoint x y _) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Eq ECPoint where
    ECPoint _ _ _ == InfinitePoint = False
    InfinitePoint == ECPoint _ _ _ = False
    InfinitePoint == InfinitePoint = True
    ECPoint x1 y1 _ == ECPoint x2 y2 _ = (x1, y1) == (x2, y2)

instance Num ECPoint where
    (+) = add
    p - q = p + (negate q)
    negate InfinitePoint = InfinitePoint
    negate (ECPoint x y curve) = ECPoint x (-y) curve

    abs = id
    (*) = undefined
    signum = undefined
    fromInteger = undefined

-- add
add :: ECPoint -> ECPoint -> ECPoint
add p InfinitePoint = p
add InfinitePoint p = p
add e@(ECPoint x1 y1 curve) (ECPoint x2 y2 _) 
    | x1 == x2 && (y1 /= y2 || (y1, y2) == (0, 0)) = InfinitePoint
    | otherwise = ECPoint {x=x, y=y, curve=curve}
    where
        diffs = (x2 - x1, y2 - y1)
        s = case diffs of
            (0, 0) -> (3 * x1 * x1 + (fromInteger $ a curve)) / (2 * y1)
            _ -> snd diffs / fst diffs
        x = s * s - x1 - x2
        y = s * (x1 - x) - y1

-- scalaMul
scalaMul :: Integer -> ECPoint -> ECPoint
scalaMul n = foldl (+) InfinitePoint . replicate n
    where
        replicate n p 
            | n == 0 = []
            | otherwise = p : replicate (n - 1) p

