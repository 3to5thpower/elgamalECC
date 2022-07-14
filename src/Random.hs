module Random (generateKeys, rand) where

import System.Random
import Data
import Data.FiniteField.Base
import qualified Data.FiniteField.PrimeField as P
import Encrypt
import Elliptic


rand :: IO Integer
rand = getStdRandom $ randomR (1000, 9999)

generateEllipticCurve :: IO EllipticCurve
generateEllipticCurve = do
    as <- (map fromInteger) <$> randomRs (1000, 9999) <$> initStdGen
    bs <- (map fromInteger) <$> randomRs (1000, 9999) <$> initStdGen
    let [(a, b)] = take 1 [(a, b) | a <- as, b <- bs, discriminant a b /= (fromInteger 0)]
    return $ EllipticCurve (P.toInteger a) (P.toInteger b)

generateBasePoint :: EllipticCurve -> IO ECPoint
generateBasePoint curve = do
    xs <- (map fromInteger) <$> randomRs (1000, 9999) <$> initStdGen
    ys <- (map fromInteger) <$> randomRs (1000, 9999) <$> initStdGen
    let [(x, y)] = take 1 [(x, y) | x <- xs, y <- ys, apply curve (ECPoint x y curve) /= (fromInteger 0)]
    return $ ECPoint x y curve

generateKeys :: IO (PublicKey, SecretKey)
generateKeys = do
    curve <- generateEllipticCurve
    basePoint <- generateBasePoint curve
    sk <- rand
    let pk = PublicKey curve basePoint (scalaMul sk basePoint)
    return $ (pk, sk)
