module EncryptSpec (spec) where

import Test.Hspec
import Random
import Elliptic
import Encrypt 

curve :: EllipticCurve
curve = EllipticCurve 49938 28872

basePoint :: ECPoint
basePoint = ECPoint 81647 64283 curve

publicKeyValue :: ECPoint
publicKeyValue = ECPoint 48782 18355 curve

publicKey :: PublicKey
publicKey = PublicKey curve basePoint publicKeyValue

secretKeyValue :: SecretKey
secretKeyValue = 89744

spec :: Spec
spec = do
    describe "encrypt and decrypt value" $ do
        it "encrypt" $ do
            encrypt 42 publicKey (ECPoint 59728 34172 curve) `shouldBe` (ECPoint 76221 45464 curve, ECPoint 80418 70377 curve)
        it "decrypt" $ do
            decrypt secretKeyValue (ECPoint 76221 45464 curve, ECPoint 80418 70377 curve) `shouldBe` ECPoint 59728 34172 curve
    describe "encrypt and decrypt string" $ do
        it "encrypt" $ do
            encryptString 42 publicKey "hello" `shouldBe`
                map (encrypt 42 publicKey) [ECPoint 104 101 curve, ECPoint 108 108 curve, ECPoint 111 0 curve]
        it "decrypt" $ do
            decryptString  secretKeyValue (map (encrypt 42 publicKey) [ECPoint 104 101 curve, ECPoint 108 108 curve, ECPoint 111 0 curve]) 
                `shouldBe` "hello"





