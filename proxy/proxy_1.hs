-- Piping operator
(|>) a f = f a

-- Function to check if number is a prime number (really inefficient)
isPrime :: Int -> Bool
isPrime a =
    a > 1 && [2 .. upper] |> any (\t -> a `mod` t == 0) |> not
    where upper = a |> fromIntegral |> sqrt |> floor

-- Proxy to check if number is a prime number (works efficiently if number is less than 50)
isPrimeProxy :: Int -> Bool
isPrimeProxy a = do
    let primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
    
    if a < 50
    then elem a primes
    else isPrime a


main :: IO ()
main = do
    putStrLn "proxy"

    isPrimeProxy 12 |> show |> ("is 12 prime : " ++) |> print
    isPrimeProxy 97 |> show |> ("is 47 prime : " ++) |> print
    isPrimeProxy 101 |> show |> ("is 52 prime : " ++) |> print
    isPrimeProxy 200 |> show |> ("is 52 prime : " ++) |> print

    putStrLn "prime numbers less than 100 according to isPrime"
    [2..100] |> filter isPrime |> show |> putStrLn

    putStrLn "prime numbers less than 100 according to isPrimeProxy"
    [2..100] |> filter isPrimeProxy |> show |> putStrLn
 
