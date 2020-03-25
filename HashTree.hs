module HashTree
  ( leaf
  , node
  , twig
  , buildTree
  , treeHash
  , drawTree
  , buildProof
  , merklePaths
  , verifyProof
  , showMerklePath
  , MerklePath
  , MerkleProof
  , Tree
  ) where
import Hashable32
import Utils

-- A

data Tree a =  Leaf Hash a | NodeOne Hash (Tree a) | NodeTwo Hash (Tree a) (Tree a)

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

node :: Hashable a => Tree a -> Tree a -> Tree a
node t1 t2 = NodeTwo (combine (treeHash t1) (treeHash t2)) t1 t2

twig :: Hashable a => Tree a -> Tree a
twig t = NodeOne (combine tHash tHash) t
  where
    tHash = treeHash t

buildTree :: Hashable a => [a] -> Tree a
buildTree [] = errorEmptyList "buildTree"
buildTree (a:[]) = Leaf (hash a) a
buildTree l = auxBuildTree (map leaf l) []

treeHash :: Tree a -> Hash
treeHash (Leaf h _) = h
treeHash (NodeOne h _) = h
treeHash (NodeTwo h _ _) = h

drawTree :: Show a => Tree a -> String
drawTree t = auxDrawTree t ""

-- A auxiliary functions
auxBuildTree :: Hashable a => [Tree a] -> [Tree a] -> Tree a
auxBuildTree [] acc
  | length acc == 1 = head acc
  | otherwise = auxBuildTree (reverse acc) []
auxBuildTree (t:[]) acc = auxBuildTree [] ((twig t):acc)
auxBuildTree (t1:t2:tTail) acc = auxBuildTree tTail ((node t1 t2):acc)

auxDrawTree :: Show a => Tree a -> String -> String
auxDrawTree (Leaf h a) indent = indent ++ showHash h ++ " " ++ show a ++ "\n"
auxDrawTree (NodeOne h t) indent =
  indent ++ showHash h ++ " +\n" ++ auxDrawTree t ("  " ++ indent)
auxDrawTree (NodeTwo h tl tr) indent =
  indent ++ showHash h ++ " -\n" ++ (auxDrawTree tl ("  " ++ indent) ++ auxDrawTree tr ("  " ++ indent))

-- | Draw Tree
-- >>> putStr $ drawTree $ buildTree "fubar"
-- 0x2e1cc0e4 -
--   0xfbfe18ac -
--     0x6600a107 -
--       0x00000066 'f'
--       0x00000075 'u'
--     0x62009aa7 -
--       0x00000062 'b'
--       0x00000061 'a'
--   0xd11bea20 +
--     0x7200b3e8 +
--       0x00000072 'r'

-- B

type MerklePath = [Either Hash Hash]
data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
    showsPrec d (MerkleProof a merklePath) = showParen (d>0) $
      showString ("MerkleProof " ++ (parentheseType a) ++ " " ++ showMerklePath merklePath)

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof a t
  | foundProofs == [] = Nothing
  | otherwise = Just $ MerkleProof a (head foundProofs)
  where
    foundProofs = merklePaths a t

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths a t = auxMerklePaths (hash a) t [] []

-- B auxiliary functions
auxMerklePaths :: Hash -> Tree a -> MerklePath -> [MerklePath] -> [MerklePath]
auxMerklePaths aHash (Leaf h _) currentPath acc
  | aHash == h = (reverse currentPath):acc
  | otherwise = acc
auxMerklePaths aHash (NodeOne h t) currentPath acc =
  auxMerklePaths aHash t ((Left $ treeHash t):currentPath) acc
auxMerklePaths aHash (NodeTwo h tl tr) currentPath acc =
  auxMerklePaths aHash tl currentPathL (auxMerklePaths aHash tr currentPathR acc)
  where
    tlHash = treeHash tl
    trHash = treeHash tr
    currentPathR = (Right tlHash):currentPath
    currentPathL = (Left trHash):currentPath

showMerklePath :: MerklePath -> String
showMerklePath merklePath = foldr f "" merklePath
  where
    f :: (Either Hash Hash) -> String -> String
    f (Left h) acc = ("<" ++ (showHash h)) ++ acc
    f (Right h) acc = (">" ++ (showHash h)) ++ acc


verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof a merklePath) = h == foldr f (hash a) merklePath
  where
    f :: (Either Hash Hash) -> Hash -> Hash
    f (Left h) acc = combine acc h
    f (Right h) acc = combine h acc

parentheseType :: Show a => a -> String
parentheseType a =
  let aShow = show a
      numberOfWords = length $ words $ aShow
      result
        | numberOfWords > 1 = "(" ++ aShow ++ ")"
        | otherwise = aShow
  in
    result

-- | B test
-- >>> mapM_ print $ map showMerklePath $ merklePaths 'i' $ buildTree "bitcoin"
-- "<0x5214666a<0x7400b6ff>0x00000062"
-- ">0x69f4387c<0x6e00ad98>0x0000006f"
--
-- >>> buildProof 'i' $ buildTree "bitcoin"
-- Just (MerkleProof 'i' <0x5214666a<0x7400b6ff>0x00000062)
--
-- >>> buildProof 'e' $ buildTree "bitcoin"
-- Nothing
--
-- >>> let t = buildTree "bitcoin"
-- >>> let proof = buildProof 'i' t
-- >>> verifyProof (treeHash t) <$> proof
-- Just True
-- >>> verifyProof 0xbada55bb <$> proof
-- Just False

-- Errors

errorEmptyList :: String -> a
errorEmptyList fun =
  errorWithoutStackTrace (prel_list_str ++ fun ++ ": empty list")

prel_list_str :: String
prel_list_str = "HashTree."
