
(define dfsnippet
  '(*haskell* "
dfsForestFrom' :: [Int] -> AdjacencyIntMap -> Forest Int
dfsForestFrom' vs g = evalState (explore vs) IntSet.empty where
  explore (v:vs) = discovered v >>= \\case
    True -> (:) <$> walk v <*> explore vs
    False -> explore vs
  explore [] = return []
  walk v = Node v <$> explore (adjacent v)
  adjacent v = IntSet.toList (postIntSet v g)
  discovered v = do new <- gets (not . IntSet.member v)
                    when new $ modify' (IntSet.insert v)
                    return new"))

(define page
  `(html
    (head
     (css-from 1)
     (meta (@ (charset "UTF-8")))
     (title "DFS alga"))
    (body
     ,dfsnippet)
    (footer)))
