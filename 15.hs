fac 1 = 1
fac n = n*(fac $ n-1)

res = (fac 40) `div` ((fac 20)*(fac 20))

