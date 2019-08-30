(define core-data-type
  '(*haskell*
    "data Graph a = Empty | Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a)"))

(define foldg-adjacency-intmap
  '(*haskell*
    "
newtype AdjacencyIntMap = AM { adjacencyIntMap :: IntMap IntSet }

empty :: AdjacencyIntMap 
empty = AM IntMap.empty

vertex :: Int -> AdjacencyIntMap
vertex x = AM $ IntMap.singleton x IntSet.empty

overlay :: AdjacencyIntMap -> AdjacencyIntMap -> AdjacencyIntMap
overlay (AM x) (AM y) = AM $ IntMap.unionWith IntSet.union x y

connect :: AdjacencyIntMap -> AdjacencyIntMap -> AdjacencyIntMap
connect (AM x) (AM y) = AM $ IntMap.unionsWith IntSet.union
    [ x, y, IntMap.fromSet (const $ IntMap.keysSet y) (IntMap.keysSet x) ]

toAdjacencyIntMap :: ToVertex t ~ Int => t -> AdjacencyIntMap
toAdjacencyIntMap = foldg empty vertex overlay connect"))

(define dfs-implementation
  '(*haskell* "
dfsForestFrom\' :: [Int] -> AdjacencyIntMap -> Forest Int
dfsForestFrom\' vs g = evalState (explore vs) IntSet.empty where
  explore (v:vs) = discovered v >>= \\case
    True -> (:) <$> walk v <*> explore vs
    False -> explore vs
  explore [] = return []
  walk v = Node v <$> explore (adjacent v)
  adjacent v = IntSet.toList (postIntSet v g)
  discovered v = do new <- gets (not . IntSet.member v)
                    when new $ modify\' (IntSet.insert v)
                    return new"))

(define top-sort-implementation
  '(*haskell* "
type Cycle = NonEmpty
data NodeState = Entered | Exited
data S = S { parent :: IntMap.IntMap Int
           , entry  :: IntMap.IntMap NodeState
           , order  :: [Int] }

topSort\' :: (MonadState S m, MonadCont m)
         => AdjacencyIntMap -> m (Either (Cycle Int) [Int])
topSort\' g = callCC $ \\cyclic ->
  do let vertices = map fst $ IntMap.toDescList $ adjacencyIntMap g
         adjacent = IntSet.toDescList . flip postIntSet g
         dfsRoot x = nodeState x >>= \\case
           Nothing -> enterRoot x >> dfs x >> exit x
           _       -> return ()
         dfs x = forM_ (adjacent x) $ \\y ->
                   nodeState y >>= \\case
                     Nothing      -> enter x y >> dfs y >> exit y
                     Just Exited  -> return ()
                     Just Entered -> cyclic . Left . retrace x y =<< gets parent
     forM_ vertices dfsRoot
     Right <$> gets order
  where
    nodeState v = gets (IntMap.lookup v . entry)
    enter u v = modify\' (\\(S m n vs) -> S (IntMap.insert v u m)
                                          (IntMap.insert v Entered n)
                                          vs)
    enterRoot v = modify\' (\\(S m n vs) -> S m (IntMap.insert v Entered n) vs)
    exit v = modify\' (\\(S m n vs) -> S m (IntMap.alter (fmap leave) v n) (v:vs))
      where leave = \\case
              Entered -> Exited
              Exited  -> error \"Internal error: dfs search order violated\"
    retrace curr head parent = aux (curr :| []) where
      aux xs@(curr :| _)
        | head == curr = xs
        | otherwise = aux (parent IntMap.! curr <| xs)"))
