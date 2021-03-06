-- The prototypical tree structure. A Description at each level and a level 
-- with information about the depth from the root
data Node = Node String Int Node
           | NodeLeaf String Int 
           | Empty
    deriving (Eq,Show)

compare_node :: Node -> Node -> Bool
compare_node a b = a == b 

-- Get the parent or nothing
unpack (Node b c d) = d
unpack (NodeLeaf b c) = Empty 
unpack Empty = Empty 

-- Accessors
getLevel (Node s i n) = i
getLevel (NodeLeaf s i) = i
getLevel Empty = 0

getString (Node s i n) = s
getString (NodeLeaf s i) = s

-- Example data for testing
myfirstNode = Node "Continuant" 2 (Node "Object" 1 (NodeLeaf "Root" 0 ))
mysecondNode = Node "Process" 2 (Node "Objict" 1  (NodeLeaf "Root" 0 ))


-- Comparison primitives
getLevels a b = map getLevel [a,b]
compareNode args = compare (head args) (last args)
compareLevels a b = compareNode (getLevels a b)

-- Traversal routines
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




