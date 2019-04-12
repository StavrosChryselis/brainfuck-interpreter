import           Data.Char
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as S
import           Data.List     (elemIndices)

data Cxt = Cxt (Seq Char) (Seq Char)
  deriving Show

type Loc = (Char,Cxt)

defaultLoc :: Loc
defaultLoc = (chr 0,Cxt Empty Empty)

maxInstructions :: Int
maxInstructions = 10^5

goLeft :: Loc -> Loc
goLeft (x,Cxt (left:|>x') right) = (x',Cxt left (x<|right))

goRight :: Loc -> Loc
goRight (x,Cxt left Empty)        = (chr 0,Cxt (left|>x) Empty)
goRight (x,Cxt left (x':<|right)) = (x',Cxt (left|>x) right)

increment :: Loc -> Loc
increment (x,c)
  | ord x == 255 = (chr 0,c)
  | otherwise = (succ x,c)

decrement :: Loc -> Loc
decrement (x,c)
  | ord x == 0 = (chr 255,c)
  | otherwise = (pred x,c)

set :: Char -> Loc -> Loc
set x (_,c) = (x,c)

isZero :: Loc -> Bool
isZero (x,_)
  | ord x == 0 = True
  | otherwise = False

printValue :: Loc -> IO ()
printValue (x,_) = putChar x

removeJunk :: String -> String
removeJunk []       = []
removeJunk ('>':xs) = '>':removeJunk xs
removeJunk ('<':xs) = '<':removeJunk xs
removeJunk ('+':xs) = '+':removeJunk xs
removeJunk ('-':xs) = '-':removeJunk xs
removeJunk ('.':xs) = '.':removeJunk xs
removeJunk (',':xs) = ',':removeJunk xs
removeJunk ('[':xs) = '[':removeJunk xs
removeJunk (']':xs) = ']':removeJunk xs
removeJunk (_:xs)   = removeJunk xs

findMatchingClosingBracket :: Int -> Map Int Char -> Int
findMatchingClosingBracket = findMatchingClosingBracket' 0
  where
    findMatchingClosingBracket' count pc instructions = case instructions M.! pc of
        ']' -> if count == 0 then pc else findMatchingClosingBracket' (count-1) (pc+1) instructions
        '[' -> findMatchingClosingBracket' (count+1) (pc+1) instructions
        _   -> findMatchingClosingBracket' count (pc+1) instructions

findMatchingOpeningBracket :: Int -> Map Int Char -> Int
findMatchingOpeningBracket = findMatchingOpeningBracket' 0
  where
    findMatchingOpeningBracket' count pc instructions = case instructions M.! pc of
      '[' -> if count == 0 then pc else findMatchingOpeningBracket' (count-1) (pc-1) instructions
      ']' -> findMatchingOpeningBracket' (count+1) (pc-1) instructions
      _ -> findMatchingOpeningBracket' count (pc-1) instructions

interpret :: Int -> Map Int Char -> Int -> String -> Loc -> IO ()
interpret pc instructions i input@(x:xs) loc =
  case M.lookup pc instructions of
      Nothing -> pure ()
      Just c -> if i > maxInstructions then putStrLn "" >> putStrLn "PROCESS TIME OUT. KILLED!!!"
                                       else case c of
        '>' -> interpret (pc+1) instructions (i+1) input $ goRight loc
        '<' -> interpret (pc+1) instructions (i+1) input $ goLeft loc
        '+' -> interpret (pc+1) instructions (i+1) input $ increment loc
        '-' -> interpret (pc+1) instructions (i+1) input $ decrement loc
        '.' -> printValue loc >> interpret (pc+1) instructions (i+1) input loc
        ',' -> interpret (pc+1) instructions (i+1) xs $ set x loc
        '[' -> if isZero loc then interpret (1+findMatchingClosingBracket (pc+1) instructions) instructions (i+2) input loc
                             else interpret (pc+1) instructions (i+1) input loc
        ']' -> if (not . isZero) loc then interpret (1+findMatchingOpeningBracket (pc-1) instructions) instructions (i+2) input loc
                                     else interpret (pc+1) instructions (i+1) input loc

main :: IO ()
main = do
    input <- getLine
    contents <- removeJunk <$> getContents
    let instructions = M.fromList $ zip [0..] contents
    interpret 0 instructions 1 input defaultLoc
