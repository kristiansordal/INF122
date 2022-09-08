pyt :: [(Integer, Integer, Integer)]
pyt = [(a, b, c) | a <- [1 .. 250], b <- [1 .. 250], c <- [1 .. 4500], a < b, a ^ 2 + b ^ 2 == c ^ 2]

initSeg :: String -> [String]
initSeg str = [take n str | n <- [0 .. length str]]
