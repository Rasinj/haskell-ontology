data Node = Node String Int Node
           | NodeLeaf String Int 
           | Empty
    deriving (Eq,Show)

compare_node :: Node -> Node -> Bool
compare_node a b = a == b 

findLCA :: Node -> Node -> Int
findLCA a b = if a == b then 1 else 0

unpack (Node b c d) = d
unpack (NodeLeaf b c) = Empty 
unpack Empty = Empty 

getLevel (Node s i n) = i
getLevel (NodeLeaf s i) = i
getLevel Empty = 0

getString (Node s i n) = s
getString (NodeLeaf s i) = s

myfirstNode = Node "Continuant" 2 (Node "Object" 1 (NodeLeaf "Root" 0 ))
mysecondNode = Node "Process" 2 (Node "Objict" 1  (NodeLeaf "Root" 0 ))

getLevels a b = map getLevel [a,b]

compareVector args = compare (head args) (last args)
compareLevels a b = compareVector (getLevels a b)

findLastCommonLevel a b = case (compareLevels a b) of
                            EQ  ->  case (getString a) == (getString b) of
                                     True -> getString a 
                                     False -> findLastCommonLevel (unpack a) (unpack b)
                            LT -> findLastCommonLevel (unpack a) b
                            GT -> findLastCommonLevel a (unpack b)

findAccLastCommonLevel e a b = case (compareLevels a b) of
                            EQ  ->  case (getString a) == (getString b) of
                                     True -> (getString a) ++ "/" ++ e 
                                     False -> findAccLastCommonLevel ((getString a) ++ "/" ++ e) (unpack a) (unpack b)
                            LT -> findAccLastCommonLevel ((getString a) ++ "/" ++ e) (unpack a) b
                            GT -> findAccLastCommonLevel ((getString a) ++ "/" ++ e) a (unpack b)




