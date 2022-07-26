module Random (generateKeys, rand, generateEllipticCurve, generateBasePoint) where

import System.Random
import Data
import Data.FiniteField.Base
import qualified Data.FiniteField.PrimeField as P
import Encrypt
import Elliptic
import Data.List ( find )


rand :: IO Integer
rand = getStdRandom $ randomR range

rands :: IO [GField]
rands = map fromInteger . randomRs range <$> initStdGen

findFromRands :: ((GField, GField) -> Bool) -> IO (GField, GField)
findFromRands predicate = do
    v1 <- fmap fromInteger rand 
    v2 <- fmap fromInteger rand
    if predicate (v1, v2)
        then return (v1,v2)
        else findFromRands predicate


generateEllipticCurve :: IO EllipticCurve
generateEllipticCurve = do
    (a, b) <- findFromRands (\(a, b) -> discriminant a b /= 0 && a /= b)
    return $ EllipticCurve (P.toInteger a) (P.toInteger b)

generateBasePoint :: EllipticCurve -> IO ECPoint
generateBasePoint curve = do
    (x, y) <- findFromRands (\(x, y) -> apply (ECPoint x y curve) == 0)
    return $ ECPoint x y curve

generateKeys :: IO (PublicKey, SecretKey)
generateKeys = do
    curve <- generateEllipticCurve
    basePoint <- generateBasePoint curve
    sk <- rand
    let pk = PublicKey curve basePoint (scalaMul sk basePoint)
    return (pk, sk)
