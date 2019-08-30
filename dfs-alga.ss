(load "style-sheet.ss")

;; links
(define snowleopard/alga "https://github.com/snowleopard/alga")
(define blog-post-link "https://github.com/snowleopard/alga#blog-posts")
(define video-link "https://www.youtube.com/watch?v=EdQGLewU-8k")
(define Data.Graph "http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Graph.html")
(define king-launchbury
  "http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=841326D58F6AE706C22F977F9864FB47?doi=10.1.1.52.6526&rep=rep1&type=pdf")
(define algebraic-graphs "http://hackage.haskell.org/package/algebraic-graphs")
(define algebraic-graphs-with-class
  "https://eprint.ncl.ac.uk/file_store/production/239461/EF82F5FE-66E3-4F64-A1AC-A366D1961738.pdf")
(define containers "http://hackage.haskell.org/package/containers")

;;; code snippets 
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

(define dfs-api
  '(*haskell* "
dfsForest :: AdjacencyIntMap -> Forest Int
dfsForest g = dfsForestFrom' (vertexList g) g

dfsForestFrom :: [Int] -> AdjacencyIntMap -> Forest Int
dfsForestFrom vs g = dfsForestFrom' [ v | v <- vs, hasVertex v g ] g

dfs :: [Int] -> AdjacencyIntMap -> [Int]
dfs vs = dfsForestFrom vs >=> flatten"))

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

(define top-sort-state
  '(*haskell* "
data NodeState = Entered | Exited
data S = S { parent :: IntMap.IntMap Int
           , entry  :: IntMap.IntMap NodeState
           , order  :: [Int] }

-- state query/manipulation
nodeState v = gets (IntMap.lookup v . entry)
enterRoot v = modify\' (\\(S m n vs) -> S m (IntMap.insert v Entered n) vs)
enter u v = modify\' (\\(S m n vs) -> S (IntMap.insert v u m)
                                      (IntMap.insert v Entered n)
                                      vs)
exit v = modify\' (\\(S m n vs) -> S m (IntMap.alter (fmap leave) v n) (v:vs))
  where leave = \\case
          Entered -> Exited
          Exited  -> error \"Internal error: dfs search order violated\""))

(define top-sort-implementation
  '(*haskell* "
type Cycle = NonEmpty

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
    retrace curr head parent = aux (curr :| []) where
      aux xs@(curr :| _)
        | head == curr = xs
        | otherwise = aux (parent IntMap.! curr <| xs)"))

(define kl-snippet
  '(*haskell*
    "
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Graph as KL

fromAdjacencyMap :: Ord a => AdjacencyMap a -> GraphKL a
fromAdjacencyMap (AM.AM m) = GraphKL
    { toGraphKL    = g
    , fromVertexKL = \\u -> case r u of (_, v, _) -> v
    , toVertexKL   = t }
  where
    (g, r, t) = KL.graphFromEdges [ ((), v, Set.toAscList us) | (v, us) <- Map.toAscList m ]"))

;; sections
(define preamble
  `((*paragraph*
     "Recently, I helped contribute to "
     (*link* "alga" ,snowleopard/alga)
     ", a cool haskell library for working with graphs developed by
Andrey Mokhov. My first contribution implemented breadth first
search. The second took ideas from the first to improve the existing
implementations of depth first search and topological sort. This post
describes the implementations and demonstrates how haskell lends
itself well to the expression of these classic algorithms.")))

(define background
  `((*section* "Background")
    (*subsection* "alga")
    (*paragraph*
     "With alga, graphs are constructed using a handful of building
blocks--from empty graphs, vertices, overlays, and
connections. Overlays union the vertex and edge sets of
graphs. Connections do the same, but include edges from each vertex of
one to each vertex of the other. The expressions thus formed may later
be instantiated to an appropriate representation for further
processing. Here is the core data type used for this algebraic
framework: "
     ,core-data-type
     "Two of the standard representations for directed graphs use
staple datastructures from "
     (*link* "containers" ,containers)
     ", "
     (mono "Data.Map a (Data.Set a)")
     " and "
     (mono "Data.IntMap Data.IntSet")
     ". Conversion to these representations (and others) is done by
specifying the appropriate fold over the interface of the graph data
type, eg:")
    ,foldg-adjacency-intmap
    (*paragraph*
     "Of course, there is much more to alga. The above hopefully gives
an idea of the library's algebra, the expression-oriented approach to
construction of graphs, and the interface used for processing
them. Alga also provides means for working with undirected graphs,
relations, acyclic graphs, bipartite graphs, and so on."
     " For more detailed information about alga, its design, and its
laws, I'd suggest checking out any of the following: "
     " a series of "
     (*link* "blog posts" ,blog-post-link) ", "
     (*link* "this talk" ,video-link) ", "
     (*link* "this paper" ,algebraic-graphs-with-class) " or the "
     (*link* "documentation" ,algebraic-graphs) "."
     )
    (*subsection* "Previously")
    (*paragraph*
     "There is a lot of power that comes from the above design. For any data type "
     (mono "g")
     " with a "
     (mono "ToGraph g")
     " instance, alga is able to use existing code from "
     (*link* "Data.Graph" ,Data.Graph)
     " simply by defining conversions between a few
representations. For example, here is enough glue to unlock existing
capabilites for graphs with "
     (mono "Ord (ToVertex g)")
     " vertices: ")
    ,kl-snippet
    (*paragraph*
     (mono "Data.Graph")
     " provides good performance for the functions it does export,
since it works on adjacency maps represented as "
     (mono "Array Int [Int]")
     ". Unfortunately, the array-based representation does not play
well with alga's algebraic coneception of graphs. For arbitrary "
     (mono "ToGraph g")
     "'s the result is three passes converting representations: "
     (enum
      (item (mono "g") " -> " (mono "AdjacencyMap a"))
      (item (mono "AdjacencyMap a") " -> " (mono "GraphKL a"))
      (item (mono "Forest Int") " -> " (mono "Forest a")
	    "  (in the case of " (mono "dfs") ")")))
    "The new implementation is basically a response to the question,
would it be better to avoid conversion by operating on adjacency maps directly?"))

(define depth-first-section
  `((*section* "Depth First Search")
    "Here is the new implementation in its entirety:"
    ,dfsnippet
    "The goal is to compute a search "
    (mono "Forest")
    " from the input vertices. It is necessary to track which vertices
have already been visited and nothing more. So, an "
    (mono "IntSet")
    " suffices, where membership of a vertex indicates that it's tree
has been explored. Here is a bottom-up explanation of the implementation:"
    (enum (item
	   (mono "adjacent v")
	   " exists because it's more pleasant to read than "
	   (mono "IntSet.toList (postIntSet v g)")
	   ".")
	  (item
	   (mono "discovered v")
	   " reports if the vertex has been discovered and marks it as
such when new.")
	  (item
	   (mono "walk v")
	   " includes the tree from "
	   (mono "v")
	   " in the forest by exploring "
	   (mono "v")
	   "'s neighbors.")
	  (item
	   (mono "explore vs")
	   " builds a forest from all unprocessed vertices in "
	   (mono "vs")
	   ".")
	  (item
	   (mono "evalState (explore vs) IntSet.empty")
	   " computes the forest from "
	   (mono "vs")
	   ". The venerable state monad is used to thread the set of
discovered vertices through the computation, driven by the mutually
recursive "
	   (mono "walk")
	   " and "
	   (mono "expore")
	   "."))
    (mono "dfsForestFrom\'")
    " has the tick \'"
    " because it is a function internal to the module. The following
three functions are exported, matching the original API from "
    (mono "Data.Graph") ":"
    ,dfs-api
    ))

(define topological-section
  `((*section* "Topological Sort")
    "The goals for implementing topological sort are loftier than
those for dfs. Besides returning a valid topological ordering of the
vertices, we want to produce the lexicographically smallest
enumeration (addressing an old "
    (*link* "issue" "https://github.com/snowleopard/alga/issues/2")
    ") or produce a cycle if there is one. These increased demands are
reflected in various ways in the implementation; more state is
threaded, a hairier monad is requested, and the compactness of dfs is
gone. All in all, it's more interesting."
    (*subsection* "State")
    "In order to produce a cycle, we need to keep track of parent
pointers, as well as if a vertex has been fully processed, or if its
descendents are currently being expanded. Also, root vertices don't
have parents, so their state must be entered differently. The state
representation used is then:"
    ,top-sort-state
    (enum
     "Brief remarks"
     (item (mono "parent")
	   " holds a table of parent pointers, "
	   (mono "entry")
	   " a table of node states, and "
	   (mono "order")
	   " the eventual topological ordering.")
     (item "the parent table is not updated when entering root nodes,
thus two different functions."))
    (*subsection* "Monad")
    ""
    

    ,top-sort-implementation))

(define alga-dfs-post
  `(html
    (head
     (*css*)
     (*haskell-css*)
     (meta (@ (charset "UTF-8")))
     (title "DFS Alga"))
    (body
     (*post-title* "Expressing graph searches in haskell")
     ,@preamble
     ,@background
     ,@depth-first-section
     ,@topological-section)))

(define (render)
  (render-page alga-dfs-post "dfs-alga.html"))

(render)

