{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Data(GField, prime) where

import Data.FiniteField.PrimeField

-- 位数pを99971とする
prime = 99971
type GField = $(primeField 99971)
