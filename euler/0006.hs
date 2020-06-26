answer upto = sqofsum - sumofsq
    where
        sumofsq = sum $ map sq nums
        sqofsum = sq $ sum nums
        nums = [1..upto]
        sq x = x * x
