module One where

import Data.List

sqr x = x * x

prime' :: Int  -> Bool
prime' x = helper 3 
  where helper :: Int -> Bool
        helper i 
          | i^2 > x = True
          | rem x i == 0 = False
          | otherwise = helper (i + 2)

suma_prima :: Int -> Int
suma_prima lim = helper 7 10
  where helper :: Int -> Int -> Int
        helper p res
          | p > lim = res
          | prime' p = helper (p + 2) (p + res)
          | otherwise = helper (p+2) res

primes = filter prime' [3,5..]

sum_primes :: Int -> Int
sum_primes lim = helper 3 2
  where helper :: Int -> Int -> Int
        helper p res
          | p > lim = res
          | (any (\x -> rem p x == 0) (takeWhile (<= plim) primes)) = helper (p + 2) res
          | otherwise = helper (p+2) (res+p)
          where plim = ceiling $ sqrt $ fromIntegral p
