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

-- 方程式の値(x^3+ax+b-y^2)を求める
apply :: EllipticCurve -> ECPoint -> GField
apply (EllipticCurve {a, b}) (ECPoint{x, y, curve}) = x * x * x + fromInteger a * x + fromInteger b - y * y
apply (EllipticCurve {a, b}) InfinitePoint = undefined

-- 判別式
discriminant :: GField -> GField -> GField
discriminant a b = 4 * a * a * a + 27 * b * b

-- 楕円上の座標を表現
data ECPoint = InfinitePoint | ECPoint {
    x :: GField,
    y :: GField,
    curve:: EllipticCurve
}

-- ECPointの文字表示形式を定義
instance Show ECPoint where
    show (ECPoint x y _) = "(" ++ show x ++ ", " ++ show y ++ ")"
    show InfinitePoint = undefined

--ECPointの等価演算を定義
instance Eq ECPoint where
    ECPoint {} == InfinitePoint = False
    InfinitePoint == ECPoint {} = False
    InfinitePoint == InfinitePoint = True
    ECPoint x1 y1 _ == ECPoint x2 y2 _ = (x1, y1) == (x2, y2)

-- ECPointの四則演算を定義(ただしabs, *, signum, fromIntegerは未定義)
instance Num ECPoint where
    (+) = add
    p - q = p + negate q
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
            (0, 0) -> (3 * x1 * x1 + fromInteger ( a curve)) / (2 * y1)
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

