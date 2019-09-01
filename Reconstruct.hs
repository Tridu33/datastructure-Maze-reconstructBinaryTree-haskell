module Reconstruct  where
data Tree a
    = Leaf
    | Node a (Tree a)(Tree a) deriving Show 

reconstruct :: Eq a=> ([a],[a]) -> Tree a
reconstruct ([],[])= Leaf
reconstruct (a:preorderArrayNoa,inorderArray)= let (treeLefta,preorderArrayRightOfa,inorderArrayRightOfa) = reconstruct' a (preorderArrayNoa,inorderArray)
           in Node a treeLefta (reconstruct(preorderArrayRightOfa,inorderArrayRightOfa))

reconstruct' :: Eq a=>a -> ([a],[a])->(Tree a,[a],[a])
reconstruct' a (preorderArrayGiven@(~(b:preorderArrayGivenNob)),ifInorderArrayRightOfa)
    | head ifInorderArrayRightOfa == a = (Leaf,preorderArrayGiven,tail ifInorderArrayRightOfa)
    | otherwise   = let (treeLeftOfb,pre_tmp,in_tmp)=reconstruct' b (preorderArrayGivenNob,ifInorderArrayRightOfa)
                        (treeRightOfb,preorderArray_Right,inorderArray_Right)=reconstruct' a (pre_tmp,in_tmp)
                   in (Node b treeLeftOfb treeRightOfb, preorderArray_Right,inorderArray_Right)

answer = reconstruct (['A','B','D','F','C','E','G','H'],['B','F','D','A','G','E','H','C'])

preorder(Leaf) = []
preorder(Node n t0 t1) = [n] ++ (preorder t0) ++ (preorder t1)
test_pre=preorder (answer)

inorder(Leaf) = []
inorder(Node n t0 t1) = (inorder t0) ++ [n] ++ (inorder t1)
test_in=inorder(answer)

postorder(Leaf) = []
postorder(Node n t0 t1) = (postorder t0) ++ (postorder t1) ++ [n]
test_post=postorder(answer)


{-
*Main> answer
Node 'A' (Node 'B' Leaf (Node 'D' (Node 'F' Leaf Leaf) Leaf)) (Node 'C' (Node 'E' (Node 'G' Leaf Leaf) (Node 'H' Leaf Leaf)) Leaf)
*Main> test_pre
"ABDFCEGH"
*Main> test_in
"BFDAGEHC"
*Main> test_post
"FDBGHECA"

-}


prettyprint :: Show a => Tree a -> [Char]
prettyprint (Leaf)
    = "Empty root."
-- unlines concats a list with newlines
prettyprint (Node node left right) = unlines (prettyprint_helper (Node node left right))
prettyprint_helper (Node node left right)
    = (show node) : (prettyprint_subtree left right)
        where
            prettyprint_subtree left right =
                ((pad "right- " "|  ") (prettyprint_helper right))
                    ++ ((pad "left- " "   ") (prettyprint_helper left))
            pad first rest = zipWith (++) (first : repeat rest)
prettyprint_helper (Leaf)
    = []
main :: IO ()
main = putStrLn $ prettyprint (answer)
{-

*Main> putStrLn $ prettyprint (answer)
'A'
+- 'C'
|  `- 'E'
|     +- 'H'
|     `- 'G'
`- 'B'
   +- 'D'
   |  `- 'F'
-}

{-


>ghci Reconstruct.hs
GHCi, version 8.6.3: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( Reconstruct.hs, interpreted )
Ok, one module loaded.
*Main>  putStrLn $ prettyprint (answer)
'A'
right- 'C'
|  left- 'E'
|     right- 'H'
|     left- 'G'
left- 'B'
   right- 'D'
   |  left- 'F'



-}
