module Random (generateKeys, rand) where

import System.Random
import Data
import Data.FiniteField.Base
import qualified Data.FiniteField.PrimeField as P
import Encrypt
import Elliptic


rand :: IO Integer
rand = getStdRandom $ randomR range

generateEllipticCurve :: IO EllipticCurve
generateEllipticCurve = do
    as <- map fromInteger . randomRs range <$> initStdGen
    bs <- map fromInteger . randomRs range <$> initStdGen
    let [(a, b)] = take 1 [(a, b) | a <- as, b <- bs, discriminant a b /= 0]
    return $ EllipticCurve (P.toInteger a) (P.toInteger b)

generateBasePoint :: EllipticCurve -> IO ECPoint
generateBasePoint curve = do
    xs <- map fromInteger . randomRs range <$> initStdGen
    ys <- map fromInteger . randomRs range <$> initStdGen
    let [(x, y)] = take 1 [(x, y) | x <- xs, y <- ys, apply curve (ECPoint x y curve) /= 0]
    return $ ECPoint x y curve

generateKeys :: IO (PublicKey, SecretKey)
generateKeys = do
    curve <- generateEllipticCurve
    basePoint <- generateBasePoint curve
    sk <- rand
    let pk = PublicKey curve basePoint (scalaMul sk basePoint)
    return $ (pk, sk)
