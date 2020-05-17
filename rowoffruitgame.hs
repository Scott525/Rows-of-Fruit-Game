-- Instruction 1
data Fruit = Apple | Pear | Orange | Plum | Banana deriving (Show, Eq, Ord) 
f1 :: [Fruit] 
f1 = [Pear, Apple, Plum] 

-- Instruction 2
data Tran = Id | Rm | Dup | Exc deriving Show 
t1 ::  [(Tran, Int)] 
t1 = [ (Rm,2), (Dup,0), (Id,0), (Exc,1) ] 

-- Instruction 3
applyTran :: (Tran,Int) -> [Fruit] -> [Fruit] 
applyTran (Id,_) xs = xs 
applyTran (Rm,n) xs = (take n xs) ++ (drop (n+1) xs) 
applyTran (Dup,n) xs = (take n xs) ++ [xs !! n] ++ (drop n xs)
applyTran (Exc,n) xs = (take n xs) ++ [xs !! (n+1)] ++ [xs !! n] ++ (drop (n+2) xs)

-- Instruction 4
applySeries :: [(Tran, Int)] -> [Fruit] -> [Fruit] 
applySeries [] xs = xs 
applySeries (tn:trans) xs = applySeries trans (applyTran tn xs)

-- Instruction 5
applyPar :: [(Tran, Int)] -> [Fruit] -> [[Fruit]]
applyPar [] xs = []
applyPar (tn:trans) xs = concat [[applyTran tn xs], (applyPar trans xs)]

-- Instruction 6
transSeq :: Tran -> Int -> [(Tran, Int)]
transSeq x y = [(x, z) | z <- [0..y]]

-- Instruction 7
transSeqAll :: Int -> [(Tran, Int)]
transSeqAll x = (transSeq Rm (x-1)) ++ (transSeq Dup (x-1)) ++ (transSeq Exc (x-2))

-- Instruction 8
initTree :: [Fruit] -> (Int, [(Int, (Tran,Int), [Fruit])]) 
initTree xs = (0, [(-1, (Id,0), xs)] ) 

-- Instruction 9
nodeParent :: (Int, (Tran,Int), [Fruit]) -> Int 
nodeParent (parent,_,_) = parent 
nodeTran :: (Int, (Tran,Int), [Fruit]) -> (Tran,Int) 
nodeTran (_,trans,_) = trans 
nodeOutcome :: (Int, (Tran,Int), [Fruit]) -> [Fruit] 
nodeOutcome (_,_,xs) = xs 

-- Instruction 10
processNodes :: [(Int, (Tran,Int), [Fruit])] -> Int -> [(Int, (Tran,Int), [Fruit])]
processNodes [] p = []
processNodes (x:xs) p = processNode x p ++ processNodes xs (p+1)

processNode :: (Int, (Tran,Int), [Fruit]) -> Int -> [(Int, (Tran,Int), [Fruit])]
processNode (x, (t,y), z) p = map (\(tnode,ynode) -> (p,(tnode,ynode),(applyTran (tnode,ynode) z))) (transSeqAll (length z))

processCurrent :: (Int, [(Int, (Tran,Int), [Fruit])]) -> (Int, [(Int, (Tran,Int), [Fruit])])
processCurrent (p, x) | p < 1     = ((p+1),x ++ processNodes x p)
                      | otherwise = ((p+1),x ++ processNodes (take 1 (drop p x)) p)
                                                             
-- Instruction 11
findFruits :: [Fruit] -> [(Int, (Tran,Int), [Fruit])] -> Int
findFruits x y = findIndex x (fruitList y)

fruitList :: [(Int, (Tran,Int), [Fruit])] -> [[Fruit]]
fruitList [] = []
fruitList (x:xs) = [(nodeOutcome x)] ++ (fruitList xs)

findIndex :: (Eq a, Ord a) => a -> [a] -> Int
findIndex n l | n `elem` l =
    let indexid :: (Eq a, Ord a) => a -> [a] -> Int -> Int
        indexid n [] _ = -1
        indexid n [x] xi = xi
        indexid n (x:xs) xi
            | n == x    = xi
            | otherwise = indexid n xs (xi+1)
    in indexid n l 0
           | otherwise = -1

-- Instruction 12
processRepeat :: Int -> [Fruit] -> (Int, [(Int, (Tran,Int), [Fruit])]) -> (Int, (Int, [(Int, (Tran,Int), [Fruit])]))
processRepeat maxiter f nt | (length (snd nt) > maxiter)     = (-1, nt)
                           | ((findFruits f (snd nt)) == -1) = processRepeat maxiter f (processCurrent nt)
                           | otherwise                       = ((findFruits f (snd nt)),nt) 

-- Instruction 13
findTransFrom :: Int -> [Fruit] -> [Fruit] -> (Int, (Int, [(Int, (Tran,Int), [Fruit])]))  
findTransFrom maxiter f1 f2 = processRepeat maxiter f2 (initTree f1)

search1 = findTransFrom 10 f1 [Pear, Plum, Pear, Apple] 

-- Instruction 14
transPath :: (Int, (Int, [(Int, (Tran,Int), [Fruit])])) -> [((Tran,Int), [Fruit])] 
transPath search1 = reverse (transPath2 (fst search1) (snd (snd search1)) )   
  where     
    transPath2 pos stree1        
      | pos == -1 = []       
      | otherwise = (nodeTran n1, nodeOutcome n1) : (transPath2 (nodeParent n1) stree1)       
      where         
        n1 = stree1!!pos 

-- Instruction 15
f2 = [Pear, Plum, Pear, Apple] 
p1 = findTransFrom 1000 f1 f2 
p2 = findTransFrom 10000 [Apple, Orange, Plum] [Apple, Orange, Orange] 
p3 = findTransFrom 10000 [Apple, Orange, Plum] [Plum, Orange, Apple, Plum] 
searchTable = findTransFrom 10000 [Apple, Pear, Banana, Orange] [Apple, Pear, Apple, Banana]