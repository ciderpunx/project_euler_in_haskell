{-# LANGUAGE PostfixOperators #-}
-- NB: Use :set -XPostfixOperators if running from GHCi

(!) :: Integer -> Integer
(!) n = product [1..n]
