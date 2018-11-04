import Data.List

data Term = Num Int
          | Add Term Term
          | Sub Term Term
          | Mul Term Term
          | Div Term Term
          deriving Show

evalTerm :: Term -> Int
evalTerm (Num i)     = i
evalTerm (Add t1 t2) = evalTerm t1 + evalTerm t2
evalTerm (Sub t1 t2) = evalTerm t1 - evalTerm t2
evalTerm (Mul t1 t2) = evalTerm t1 * evalTerm t2
evalTerm (Div t1 t2) = evalTerm t1 `div` evalTerm t2

formatTerm :: Term -> String
formatTerm (Num i)     = show i
formatTerm (Add t1 t2) = "(" ++ formatTerm t1 ++ " + " ++ formatTerm t2 ++ ")"
formatTerm (Sub t1 t2) = "(" ++ formatTerm t1 ++ " - " ++ formatTerm t2 ++ ")"
formatTerm (Mul t1 t2) = "(" ++ formatTerm t1 ++ " * " ++ formatTerm t2 ++ ")"
formatTerm (Div t1 t2) = "(" ++ formatTerm t1 ++ " / " ++ formatTerm t2 ++ ")"

splitlist :: [Int] -> [([Int], [Int])]
splitlist xs = zip ys $ map (xs \\) ys
    where ys = init . tail $ subsequences xs

terms :: [Int] -> [Term]
terms []  = []
terms [x] = [Num x]
terms xs  = foldr (++) [] $ map terms' $ splitlist xs
    where terms' :: ([Int], [Int]) -> [Term]
          terms' (ls, rs) = lt ++ rt ++ (foldr (++) [] [combineTerms l r | l <- lt, r <- rt])
              where lt = terms ls
                    rt = terms rs
                    combineTerms l r = [Add l r, Sub l r, Mul l r, Div l r]

validTerm :: Term -> Bool
validTerm (Num i) = i > 0
validTerm (Add t1 t2) = validTerm t1 && validTerm t2
validTerm (Mul t1 t2) = validTerm t1 && validTerm t2
validTerm (Div t1 t2) = validTerm t1 && validTerm t2 && let v2 = evalTerm t2 
                                                         in v2 > 0 && let v1 = evalTerm t1
                                                                       in mod v1 v2 == 0
validTerm (Sub t1 t2) = validTerm t1 && validTerm t2 && evalTerm t1 > evalTerm t2

solve :: Int -> [Int] -> [Term]
solve target numbers = filter isSolution $ filter validTerm $ terms numbers
    where isSolution term = target == evalTerm term

main :: IO ()
main = do
    putStrLn "Target:"
    target <- getLine
    putStrLn "Numbers:"
    numbers <- getLine
    putStrLn "Solution:"
    putStrLn . formatTerm . head $ solve (read target) (read $ "[" ++ numbers ++ "]")
