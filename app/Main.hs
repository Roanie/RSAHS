module Main where

import Lib
import System.Random
import Text.Printf

main :: IO ()
main = do
    putStrLn "Press enter to generate key-pair"

    let allPrimes = primes (10^24) -- Arbitrarily large

    randoms <- randomInts 2 (5 * 10^2) (2 * 10^3)

    let p = allPrimes!!(randoms!!0)
    let q = allPrimes!!(randoms!!1)

    printf "p = %d\nq = %d\n" p q

    let n = p * q
    let m = (p - 1) * (q - 1)

    printf "n = %d\nm = %d\n" n m
    
    let e = getE m 
    let d = getD e m 

    printf "e = %d\nd = %d\n\n" e d

    printf "public key: X^%d (mod %d)\n" e n
    printf "private key: X^%d (mod %d)\n" d n

    putStrLn "Enter a number to be encrypted"
    resNum <- getLine
    let num = read resNum :: Integer
    let encrypted = rsa num e n
    printf "Encrypted: %d\n" encrypted 
    let decrypted = rsa encrypted d n 
    printf "Decrypted: %d\n" decrypted


-- Simply performs the encryption calculation x^(e or d) (mod n)
rsa :: (Integral a) => a -> a -> a -> a
rsa x ed n = (x^ed) `mod` n

-- returns an encryption and decryption function for RSA
rsaFuncs :: IO((Integer -> Integer, Integer -> Integer) )
rsaFuncs = do
    (n, e, d) <- keyPair 
    return ((\x -> rsa x e n),(\x -> rsa x d n))

-- returns a public and private rsa key pair, arg0 = n, arg1 = e, arg2 = d
keyPair :: (Integral a) => IO((a, a, a))
keyPair = do
    let allPrimes = primes (10^24)
    randoms <- randomInts 2 (5 * 10^2) (2 * 10^3)
    let p = allPrimes!!(randoms!!0)
    let q = allPrimes!!(randoms!!1)
    let n = p * q
    let m = (p - 1) * (q - 1)
    let e = getE m 
    let d = getD e m 
    return (n, e, d)

-- Returns a pair of random ints, the amount depens on n, the range is l..u
randomInts :: Int -> Int -> Int -> IO[Int]
randomInts n l u = do
    g <- newStdGen
    return $ take n $ randomRs (l, u) g

-- IntegerSquareRoot, returns the floor integer value to a sqrt
isqrt :: (Integral a) => a -> a
isqrt = floor . sqrt . fromIntegral

-- Checks weather an integer is a prime
isPrime :: (Integral a) => a -> Bool
isPrime n = length [x | x <-[2..(isqrt n)], n `mod` x == 0] == 0

-- Returns all primes up to and including n and returns the primes found
primes :: (Integral a) => a -> [a]
primes n = 2:[x | x <- [3, 5..n], isPrime x]

-- SGD, finds the greatest common divisors
sgd :: (Integral a) => a -> a -> a
sgd a b
    | b == 0 = a
    | otherwise = sgd b (a `mod` b)

-- Extended sgd, aka Extended Euclidean algorithm, returns the integer factor to the two integers a b to construct the greatest common divisor
esgd :: (Integral a) => a -> a -> (a, a, a)
esgd 0 b = (b, 0, 1)
esgd a b = let (g, s, t) = esgd (b `mod` a) a
           in (g, t - (b `div` a) * s, s)

-- Gets the e in RSA encryption
getE :: (Integral a) => a -> a
getE m = case eTest m of
    Just x -> x
    Nothing -> eCalc 2 m

-- manually checks which value e should be 
eCalc :: (Integral a) => a -> a -> a
eCalc n m 
    | sgd n m == 1 = n
    | otherwise = eCalc (n + 1) m

-- checks a few special primes and see if they work as e with this particualr m
eTest :: (Integral a) => a -> Maybe a
eTest m 
    | length result == 0 = Nothing
    | otherwise = Just $ result!!0
        where result = [x | x <- [3, 5, 17, 257, 65537], x `sgd` m == 1] -- the list are special primes, and they are commonly a resultion to sgd e m == 1

-- returns the d value in RSA for a givin e and m. This is done via the extended euclidean algorithm
getD :: (Integral a) => a -> a -> a
getD e m 
    | x < m = modifyUntil x (\a -> a + m) (\a -> a < m && a > 1)
    | otherwise = modifyUntil x (\a -> a - m) (\a -> a < m && a > 1)
        where (v, x, y) = esgd e m

-- performs a function unitl a predicate returns true and then returns the value which the function returns
modifyUntil :: a -> (a -> a) -> (a -> Bool) -> a
modifyUntil v fn cmp 
    | cmp v = v
    | otherwise = modifyUntil (fn v) fn cmp 
