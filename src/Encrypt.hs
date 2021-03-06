module Encrypt (PublicKey (PublicKey, g, key), SecretKey, encryptString, decryptString, encrypt, decrypt) where

import Data.Char
import Data.FiniteField.Base
import qualified Data.FiniteField.PrimeField as P
import Elliptic
import GHC.Num.Integer

-- 公開鍵および秘密鍵のデータ型を定義
data PublicKey = PublicKey
    { curve :: EllipticCurve,
        g :: ECPoint,
        key :: ECPoint
    }

type SecretKey = Integer

instance Show PublicKey where
    show (PublicKey curve g key) = show key ++ ", 楕円曲線が " ++ show curve ++ ", 生成元Gは" ++ show g


-- encrypt function required random gfield value r
encrypt :: Integer -> PublicKey -> ECPoint -> (ECPoint, ECPoint)
encrypt r pk m = (c1, c2)
    where
        c1 = scalaMul r (g pk)
        c2 = m + scalaMul r (key pk)

-- decrypt function
decrypt :: SecretKey -> (ECPoint, ECPoint) -> ECPoint
decrypt sk (c1, c2) = c2 - scalaMul sk c1

encryptString :: Integer -> PublicKey -> String -> [(ECPoint, ECPoint)]
encryptString r pk msg = map (encrypt r pk) points
    where
        points = map (\(x, y) -> ECPoint (fromInteger x) (fromInteger y) (curve pk)) (pairs integers)
        integers = map (toInteger . ord) msg
        pairs [] = []
        pairs [x] = [(x, 0)]
        pairs (x : y : rest) = (x, y) : pairs rest

decryptString :: SecretKey -> [(ECPoint, ECPoint)] -> String
decryptString sk = concatMap (toChars . decrypt sk)
    where
        toChars (ECPoint x 0 _) = [toChar x]
        toChars (ECPoint x y _) = toChar x : [toChar y]
        toChars InfinitePoint = undefined
        toChar = chr . integerToInt . P.toInteger
