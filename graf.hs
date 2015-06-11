import Data.Graph
import Data.List


graph tab = buildG (1,7) tab
--makeGraph =  
--
subsets 0 _ = [[]]
subsets _ [] = [[]]
subsets k (x:xs) = map (x:) (subsets (k - 1) xs) ++ subsets k xs

sub  = subsets 8 [8..17]

--mg = graph sub
arrayToEdges _ [] = []
arrayToEdges i (x:xs) = [(i,x)]++(arrayToEdges i xs)

podzbiory [] = [[]]
podzbiory (x:xs) = (map (x:) (podzbiory xs)) ++ (podzbiory xs)



--wybory x y 1 = y
--wybory::[[Int]]->[[Int]]->Int->[[[Int]]]
wybory [] _ = [[]]
wybory _ 0 = [[]]
wybory (x:xs) i = (map (x:) (wybory (filter (isS x) xs) (i-1))) ++ (wybory xs i)



make1 [] = [[]]
make1 (x:xs) = (make2 x 1) : (make1 xs)
make2 [] _ = []
make2 (x:xs) i = (arrayToEdges i x):(make2 xs (i+1)) 


s = filter (\x -> (length x < 5) && ( length x > 1)) sub
s2 = filter (\x -> (length x <=2)) sub

isS x y = (isSubset x y y) && (isSubset y x x)
isSubset [] _ _= False
isSubset _ [] _= True
isSubset (x:xs) (y:ys) yy | x==y = (isSubset xs yy yy)
  | otherwise = (isSubset (x:xs) ys yy) 

wyn x = filter (\y -> length y == x) (wybory s2 x)

  

test x = head $ wyn x

usunMulti [] = []
usunMulti (x:y:xs) | x==y = usunMulti (y:xs)
  | otherwise = x:(usunMulti (y:xs))
usunMulti x = x

--bierze tablicę intów i idzie po kolei i z drugiej tablicy bierze rekordy z pozycji na pierwszej
koloruj [] _ = []
koloruj (x:xs) y = (last $ take x y)++(koloruj xs y)

--co w zbiorze y jest czego nei ma w zbiorze x
dopelnienie [] y = y
dopelnienie (x:xs) (y:ys) | x==y = dopelnienie xs ys
  | otherwise = y:(dopelnienie (x:xs) ys)

contains _ [] = False
contains x (y:ys) | x==y = True
  | otherwise = (contains x ys)

reszta [] _ = True
reszta (x:xs) y | (contains x y) = reszta xs y
  | otherwise = False 

resztaC [] _ min = min
resztaC ((x,m):xs) y min | ((reszta y x) && (m<min)) = resztaC xs y m
  | otherwise = (resztaC xs y min)

--co jest juz pokolorowane i co nie jest
poko x y= usunMulti $ sort $ koloruj x y
dopPoko x y = dopelnienie (poko x y) [1..4]

pokoAll [] _ = []
pokoAll (x:xs) y = ((poko x y),length x):(pokoAll xs y)

--sortuje w taki sposób, że najpierw na liście są najdluższe, potem coraz krótsze itp
sortL::[[Int]]->Int->[[Int]]
sortL _ 0 = []
sortL x k = (filter (\y -> length y == k) x)++(sortL x (k-1))

minReszta x y = resztaC (pokoAll ((filter (\z->(length z> 0)) (subsets 4 [1..4])) )y) (dopPoko x y) 20


graf = [[1,2],[1],[1,3],[4]]
y=[1,2]
ti = minReszta y graf
pom::[[Int]]
pom = filter (\z->(length z> 0)) (subsets 4 [1..4])
p2 = pokoAll pom graf

