module Main where

import Data
import Encrypt
import Random

main :: IO ()
main = do
    putStrLn $ "位数は" ++ (show prime) ++ "です。"
    app


app :: IO ()
app = do
    (pk, sk) <- generateKeys
    putStrLn $ "今回の秘密鍵は " ++ (show sk) ++ ", 公開鍵は" ++ (show pk) ++ "です。"
    putStrLn "暗号化したい文字列を入力してください。"
    r <- rand
    msg <- getLine
    let e = encryptString r pk msg
    putStrLn $ "暗号化結果は" ++ (show e) ++ "です。"
    putStrLn $ "復号化結果は" ++ (show $ decryptString sk e) ++ "です。"
    app

