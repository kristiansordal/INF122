import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.UTF8  as UTF8
import Data.List
import qualified Data.Map as Map
import Model
import NGram
import System.Environment
import System.IO
import System.Random
import Text.Read (readMaybe)

-- Print the usage instructions for the program.
printUsage =
  putStrLn $
    "Usage: ramble <COMMAND> <MODELFILE> [SAMPLEFILE]\n"
      ++ "where COMMAND is one of:\n"
      ++ "   create:  Create a new model from samplefile or stdin\n"
      ++ "   on <STARTPRASE> <LENGTH>:\n"
      ++ "                Generate text from model starting\n"
      ++ "                from STARTPHRASE until total length\n"
      ++ "                reaches LENGTH.\n\n"
      ++ "Examples:\n"
      ++ "    ramble create alice.mod alice.txt\n"
      ++ "    ramble on \"Alice went\" alice.mod"

-- A global n for our n-grams.
-- If you play around with it, remember that it
-- has to be the same when creating and using a model.
-- All example models are generated with gramLen = 7
gramLen :: (Num a) => a
gramLen = 7

-- Pick out an element from a weighted list by
-- going through the list until a certain treshold has been
-- reached.
pick :: [(a, Weight)] -> Weight -> a
pick weights treshold
  -- continue recursion while the treshhold is greater than or equal to the weight of the head
  -- subtract the weight of the head from the treshold if this case is true.
  -- return the first component of the head if the treshold is exceeded
  | snd (head weights) <= fromIntegral treshold = pick (tail weights) (treshold - snd (head weights))
  | otherwise = fst $ head weights

-- Pick a random element from a weighted list with a given
-- total weight.
pickRandom :: [(a, Weight)] -> Weight -> IO a
pickRandom wl total =
  do
    -- get random number with '<-' to remove IO side effect
    rand <- randomRIO (0, total - 1)
    return $ pick wl rand

-- Generate a fixed amount of text from a model starting from a given
-- start string
generate :: TextModel -> String -> Integer -> IO String
generate model start amount =
  do
    let lastGram = last $ grams gramLen start
    genGram <- generate' model lastGram amount
    -- get the new start value by taking the element in start at the index obtained by
    -- subtracting the gramLen from the length of start and adding 1
    let start' = take (length start - gramLen + 1) start
    return $ take (fromIntegral amount) (start' ++ combineGrams genGram)

-- Helper function which generates n-grams from a model
generate' :: TextModel -> NGram -> Integer -> IO [NGram]
generate' model start 0 = return []
generate' model start amount =
  -- nextDistributions returns a maybe, so we put it in a case statement
  case nextDistribution model start of
    -- unpack return value to a tuple
    Just (list, weight) ->
      do
        -- get next element, then recurse until amount reaches 0
        next <- pickRandom list weight
        nexts <- generate' model next (amount - 1)
        return (next : nexts)
    Nothing -> error "No next distribution for the given arguments"

-- Serialize a text model and write a handle.
writeModel :: TextModel -> Handle -> IO ()
writeModel model h =
  ByteString.hPut h $
    GZip.compress $
      UTF8.fromString $
        show model

-- Read a text model from a handle.
readModel :: Handle -> IO TextModel
readModel h =
  -- fmap over the functor (monad, to be more specific) returned by ByteString.hGetContents
  fmap (read . UTF8.toString . GZip.decompress) (ByteString.hGetContents h)

main = do
  args <- getArgs
  case args of
    ["create", modelFile] -> do
      modelh <- openFile modelFile WriteMode
      sample <- hGetContents stdin
      let model = createModel gramLen sample
      writeModel model modelh
      hClose modelh
    ["create", modelFile, sampleFile] -> do
      modelh <- openFile modelFile WriteMode
      sampleh <- openFile sampleFile ReadMode
      sample <- hGetContents sampleh
      let model = createModel gramLen sample
      putStrLn $ "Created model with: " ++ show (Map.size model) ++ " n-grams"
      writeModel model modelh
      hClose modelh
      hClose sampleh
    ["on", startPhrase, sLength, modelFile] -> do
      modelh <- openFile modelFile ReadMode
      model <- readModel modelh
      case readMaybe sLength of
        (Just outlength) ->
          generate model startPhrase outlength >>= putStrLn
        Nothing -> printUsage
      hClose modelh
    _ -> printUsage
