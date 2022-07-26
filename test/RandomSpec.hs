module RandomSpec(spec) where

import Test.Hspec
import Random 
import Elliptic 
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import GHC.Generics (Generic(from))
import Control.Monad.Trans
import Encrypt


shouldReturnSatisfy :: IO a -> (a -> Bool) -> Expectation
action `shouldReturnSatisfy` p = action  >>= (`shouldBe` True) . p

spec:: Spec 
spec = do
    describe "EllipticCurve" $ do
        it "discriminant"  $ do
            generateEllipticCurve `shouldReturnSatisfy` (\(EllipticCurve a b) -> discriminant (fromInteger a) (fromInteger b) /= 0)
    describe "EllipticBasePoint" $ do
        it "apply value" $ do
            (generateEllipticCurve >>= generateBasePoint) `shouldReturnSatisfy` (\point -> apply point == 0) 
    describe "generate keys" $ do
        it "keys" $ do
            generateKeys `shouldReturnSatisfy`
                (\(PublicKey _ basePoint pk, sk) -> scalaMul sk basePoint == pk)



