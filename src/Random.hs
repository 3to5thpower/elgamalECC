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

generateEllipticCurve :: IO EllipticCurve
generateEllipticCurve = do
    randoms <- map fromInteger . randomRs range <$> initStdGen
    let Just(a, b) = find (\(a, b) -> discriminant a b /= 0 && a /= b) [(a, b) | a <- randoms, b <- randoms]
    return $ EllipticCurve (P.toInteger a) (P.toInteger b)

generateBasePoint :: EllipticCurve -> IO ECPoint
generateBasePoint curve = do
    randoms <- map fromInteger . randomRs range <$> initStdGen
    let Just(x, y) = find (\(x, y) -> apply (ECPoint x y curve) == 0)  [(x, y) | x <- randoms, y <- randoms]
    return $ ECPoint x y curve

generateKeys :: IO (PublicKey, SecretKey)
generateKeys = do
    curve <- generateEllipticCurve
    basePoint <- generateBasePoint curve
    sk <- rand
    let pk = PublicKey curve basePoint (scalaMul sk basePoint)
    return (pk, sk)
