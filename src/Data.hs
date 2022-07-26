{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Data(GField, prime, range) where

import Data.FiniteField.PrimeField ( primeField )

-- 位数pを99971とする
prime = 99971
type GField = $(primeField 99971)

-- 乱数の範囲を定義
range = (1,99999)
