module Main where

import Lib
import System.Random

main :: IO ()
main = do
    putStrLn "Press enter to generate key-pair"

    let allPrimes = primes (10^10) -- Arbitrarily large

    randoms <- randomInts 2 (5 * 10^2) (2 * 10^3)

    let p = allPrimes!!(randoms!!0)
    let q = allPrimes!!(randoms!!1)

    putStr "(p, q) = "
    putStrLn $ show (p, q)

    let n = p * q
    let m = (p - 1) * (q - 1)

    putStr "n = "
    putStrLn $ show n
    putStr "m = "
    putStrLn $ show m
    
    

    putStrLn ""


randomInts :: Int -> Int -> Int -> IO[Int]
randomInts n l u = do
    g <- newStdGen
    return $ take n $ randomRs (l, u) g

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime n = length [x | x <-[2..(isqrt n)], mod n x == 0] == 0

primes :: Int -> [Int]
primes n = 2:[x | x <- [3, 5..n], isPrime x]

sgd :: Int -> Int -> Int
sgd a b
    | b == 0 = a
    | otherwise = sgd b (mod a b)

e :: Int -> Int -> Int
e n m 
    | sgd n m == 1 = n
    | otherwise = e (n + 1) m

eCommon :: Int -> Maybe Int
eCommon m 
    | length soult == 0 = Nothing
    | otherwise = Just $ soult!!0
        where soult = [x | x <- [3, 5, 17, 257, 65537], x `sgd` m == 1] -- the list are special primes, and they are commonly a soultion to sgd e m == 1

d :: Int -> Int