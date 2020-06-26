isPrime x = all (\y -> x `mod` y /= 0) [2..div x 2]
answer n = primes !! (n - 2)
    where primes = filter isPrime [3,5..]
