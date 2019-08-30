(load "style-sheet.ss")

(define *title* "Expressing graph searches in haskell")
(define snowleopard/alga "https://github.com/snowleopard/alga")
(define blog-post-link "https://github.com/snowleopard/alga#blog-posts")
(define video-link "https://www.youtube.com/watch?v=EdQGLewU-8k")
(define Data.Graph "http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Graph.html")
(define containrs "http://hackage.haskell.org/package/containers-0.6.2.1")
(define core-data-type
  '(*haskell*
    "data Graph a = Empty | Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a)"))

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
                    return new
     "))

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
        | otherwise = aux (parent IntMap.! curr <| xs)
              "))



(define Content
  `((*title* ,*title*)
    (*paragraph*
     "Recently, I helped contribute to "
     (*link* "alga" ,snowleopard/alga)
     ", a fun haskell library for working with graphs developed by
Andrey Mokhov. My first contribution implemented breadth first
search. The second took ideas from the first to improve the existing
implementations of depth first search and topological sort. This post
describes the implementations and how haskell lends itself well to the
expression of these classic algorithms.")
    (*section* "Backround on alga")
    (*paragraph*
     "Graphs are constructed using a handful of building blocks, from
empty graphs, vertices, overlays, and connections. Overlays union the
vertex and edge sets of graphs. Connections do the same, but include
edges from each vertex of one to each vertex of the other. The
expressions thus formed may later be instantiated to an appropriate
representation for further processing. Here is the core data type used
for this algebraic framework: "
     ,core-data-type
     "Two of the standard representations for directed graphs use
staple datastructures from "
     (*link* "containers" ,containrs)
     ", "
     (mono "Data.Map a (Data.Set a)")
     " and Data.IntMap Data.IntSet. Careful use of compiler directives
to achieve fusion as well as good merge complexeties of these
datastructures mean that alga excels in graph construction
benchmarks.")
    (*paragraph*
     " More information about alga, its design, and its laws may be
found in a series of "
     (*link* "blog posts" ,blog-post-link) " and in "
     (*link* "this talk" ,video-link) ".")

    (*section* "Depth First Search")
    "Previously, alga wrapped the graph type from "
    (*link* "Data.Graph" ,Data.Graph)
    " to take advantage of existing the dfs implementation."
    ,dfs-implementation
    (*section* "Topological Sort")
    ,top-sort-implementation
    ))

(define alga-dfs-post
  `(html
    (head
     (*haskell-css*)
     (meta (@ (charset "UTF-8")))
     (title "DFS Alga"))
    (body ,@Content)))

(render-page alga-dfs-post "dfs-alga.html")

