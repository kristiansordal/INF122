import Data.Array
import Data.List.Extra
import Data.Set (Set)
import Data.Set qualified as Set

main = do
  input <- map parse . lines <$> readFile "day8.in"
  let array = listArray (0, length input - 1) input
      ansP1 = acc array Set.empty 0 0
  print ansP1

type Instruction = (String, Integer)

-- changeArr :: Array Int Instruction -> Array Int Instruction ->Integer
-- changeArr arr orig =
--   case acc' arr Set.empty 0 0 of
--     (Just ix) = case fst $ arr ! ix of
--                 "acc" = changeArr
--                 "nop"
--                 "jmp"
--     (Nothing) = acc' arr Set.empty 0 0

-- acc' :: Array Int Instruction -> Set (Instruction, Int) -> Int -> Integer -> Maybe Integer
-- acc' arr set ix x
--   | ix == fromIntegral (length arr + 1) = Nothing
--   | Set.member (curr, ix) set = Just $ toInteger ix
--   | otherwise =
--       case instruction of
--         "acc" -> acc' arr (Set.insert (curr, ix) set) (ix + 1) (x + fromIntegral val)
--         "nop" -> acc' arr (Set.insert (curr, ix) set) (ix + 1) x
--         "jmp" -> acc' arr (Set.insert (curr, ix) set) (ix + fromIntegral val) x
--   where
--     curr = arr ! ix
--     instruction = fst curr
--     val = snd curr

acc :: Array Int Instruction -> Set (Instruction, Int) -> Int -> Integer -> Integer
acc arr set ix x
  | Set.member (curr, ix) set = x
  | otherwise =
      case instruction of
        "acc" -> acc arr (Set.insert (curr, ix) set) (ix + 1) (x + fromIntegral val)
        "nop" -> acc arr (Set.insert (curr, ix) set) (ix + 1) x
        "jmp" -> acc arr (Set.insert (curr, ix) set) (ix + fromIntegral val) x
  where
    curr = arr ! ix
    instruction = fst curr
    val = snd curr

parse :: String -> Instruction
parse s
  | head (snd tup) == '+' =
      let num = read (tail $ snd tup)
       in (fst tup, num)
  | otherwise =
      let num = read $ snd tup
       in (fst tup, num)
  where
    tup = word1 (fst $ line1 s)
