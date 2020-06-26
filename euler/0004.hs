answer = maximum [ p | a <- [100..999], b <-[100..a], let p = a * b, palindrome p ]
palindrome n = let s = show n in s == reverse s
