
tens 1 = "ten"
tens 2 = "twenty"
tens 3 = "thirty"
tens 4 = "forty"
tens 5 = "fifty"
tens 6 = "sixty"
tens 7 = "seventy"
tens 8 = "eighty"
tens 9 = "ninety"

nTS 0 = ""
nTS 1 = "one"
nTS 2 = "two"
nTS 3 = "three"
nTS 4 = "four"
nTS 5 = "five"
nTS 6 = "six"
nTS 7 = "seven"
nTS 8 = "eight"
nTS 9 = "nine"
nTS 10 = "ten"
nTS 11 = "eleven"
nTS 12 = "twelve"
nTS 13 = "thirteen"
nTS 15 = "fifteen"
nTS 18 = "eighteen"
nTS 1000 = "onethousand"
nTS x | x > 10   && x < 20  = nTS (x `rem` 10) ++ "teen"
      | x >= 20  && x < 100 = tens (x`div`10) ++ nTS (x `rem` 10)
      | x >= 100 && x < 1000 = nTS (x `div` 100) ++ "hundred" ++ if x `rem` 100 /= 0
                                                                 then "and" ++ nTS (x `rem` 100)
                                                                 else ""

res = sum . map (length . nTS) $ [1..1000]
