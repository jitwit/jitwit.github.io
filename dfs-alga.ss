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

(define real-world-graphs-summary
  (let ((data '((n1 21 15)
		(n2 404 87)
		(n3 3228 349)
		(n4 26703 1628)
		(n5 57949 3487)
		(nodes4000 40000 4000)
		(nodes7000 70000 7000)
		(nodes8000 80000 8000)
		(nodes10000 100000 10000))))
    `(table (caption "Sizes of the haskell-perf/graphs")
	    '(tr (th (b (mono "G")))
		 (th (@ (align "right"))
		     (b (mono "|E|")))
		 (th (@ (align "right"))
		     (b (mono "|V|"))))
	    ,@(map (lambda (row)
		     `(tr ,@(map (lambda (r)
				   (if (number? r)
				       `(td (@ (align "right"))
					    ,(number->string r))
				       `(td (@ (align "left"))
					    ,(symbol->string r))))
				 row)))
		   data))))

(define criterion-tables
  (let* ((data (with-input-from-file "~/code/alga-bench/alga-bench.ss" read))
	 (benches (map car data))
	 (points (map cdr data))
	 (libs '("new-alga" "old-alga" "kl" "fgl")))
    `(table (caption "Average relative performance over haskell-perf graphs")
	    (tr (th "")
		,@(map (lambda (lib)
			 `(th (@ (align "right"))
			      ,lib))
		       libs))
	    ,@(map (lambda (bench point)
		     `(tr (td  (@ (align "left"))
			       ,(substring bench 0 (- (string-length bench) 6)))
			  ,@(map (lambda (lib)
				   (cond ((assoc lib point)
					  => (lambda (res)
					       `(td (@ (align "right"))
						    ,(format "~,2f" (cdr res)))))
					 (else `(td (@ (align "right"))
						    "n/a"))))
				 libs)))
		   benches
		   points))))

;;; code snippets 
(define core-data-type
  '(*haskell*
    "data Graph a = Empty | Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a)"))

(define foldg-tograph
  '(*haskell*
    "
newtype AdjacencyMap a = AM { adjacencyMap :: Map a (Set a) }

empty :: Ord a => AdjacencyMap a 
empty = AM Map.empty

vertex :: Ord a => a -> AdjacencyMap a
vertex x = AM $ Map.singleton x Set.empty

overlay :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
overlay (AM x) (AM y) = AM $ Map.unionWith Set.union x y

connect :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
connect (AM x) (AM y) = AM $ Map.unionsWith Set.union
    [ x, y, Map.fromSet (const $ Map.keysSet y) (Map.keysSet x) ]

toAdjacencyMap :: Ord (ToVertex g) => g -> AdjacencyMap a
toAdjacencyMap = foldg empty vertex overlay connect"))

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

(define topo-state-type
  '(*haskell* "dfs :: (MonadState S m, MonadCont m) => Int -> m ()"))
(define dfs-state-type
  '(*haskell* "explore :: State IntSet (Forest Int)"))

(define top-sort-state
  '(*haskell* "
nodeState v = gets (IntMap.lookup v . entry)

enterRoot v = modify\' (\\(S m n vs) -> S m (IntMap.insert v Entered n) vs)

enter u v = modify\' (\\(S m n vs) -> S (IntMap.insert v u m)
                                      (IntMap.insert v Entered n)
                                      vs)

exit v = modify\' (\\(S m n vs) -> S m (IntMap.alter (fmap leave) v n) (v:vs))
  where leave = \\case
          Entered -> Exited
          Exited  -> error \"Internal error: dfs search order violated\""))

(define top-sort-types
  '(*haskell* "
data NodeState = Entered | Exited

data S = S { parent :: IntMap.IntMap Int
           , entry  :: IntMap.IntMap NodeState
           , order  :: [Int] }"))

(define top-sort-cycle
  '(*haskell*
    "type Cycle = NonEmpty

retrace :: Int -> Int -> IntMap Int -> Cycle Int
retrace curr head parent = aux (curr :| []) where
  aux xs@(curr :| _)
    | head == curr = xs
    | otherwise = aux (parent IntMap.! curr <| xs)"))


(define top-sort-core
  '(*haskell* "
topSort\' :: (MonadState S m, MonadCont m)
         => AdjacencyIntMap
         -> m (Either (Cycle Int) [Int])
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
     Right <$> gets order"))

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
     ", a haskell library for working with graphs developed by Andrey
Mokhov. My first contribution implemented breadth first search. The
second took ideas from the first to improve the existing
implementations of depth first search and topological sort. This post
describes the implementations and illustrates how these classic
algorithms may be expressed in haskell. I'll also present evidence
that the implementations are effective in the form of criterion
benchmarks. For now it suffices to know that the main representation
alga uses for directed graphs is adjacency maps in the form of "
     (mono "Map a (Set a)")
     " or "
     (mono "IntMap IntSet")
     ".")))

(define background
  `((*section* "Alga Background")
    (*subsection* "Core Idea")
    (*paragraph*
     "Alga deals with the construction and manipulation of
graphs. A (di-) graph is some object with a set of vertices and a set
edges, (ordered) pairs of vertices. The main way that alga expresses
this is with the following data type: "
     ,core-data-type
     "Vertices are drawn from elements of some type "
     (mono "a")
     " and wrapped by the "
     (mono "Vertex")
     " constructor. "
     (mono "Overlay g h")
     " is the graph whose vertex and edge sets are the unions of "
     (mono "g")
     "'s and "
     (mono "h")
     "'s. "
     (mono "Connect g h")
     " is the graph whose vertex and edge sets contain those from
overlay, as well as an edge for every vertex of "
     (mono "g")
     " to every vertex of "
     (mono "h")
     " . This construction is now visibly algebraic in the sense that
there are two closed operations for building graphs from other
graphs, "
     (mono "Overlay")
     " and "
     (mono "Connect")
     ", which in fact satisfy axioms analogous to "
     (mono "+")
     " and "
     (mono "*")
     ". In fact alga provides a cute "
     (mono " Num ")
     " instance, so that expressions like "
     (mono " 1*2 ")
     " can be used to construct an edge from 1 to 2 (note, * is
overlay). It's also cute in the sense that it's not lawful. "
     (mono " Num ")
     " is usually intended for Rings, where the identities for + and *
are distinct. Here, both overlay and connect have "
     (mono "Empty")
     " as identity.")
    (*paragraph*
     "The "
     (mono "Graph")
     " data type can be viewed as a DSL for graph construction. Alga
also defines an encoding for this construction as a type class "
     (mono "Graph g")
     ", which requires analogues for the type constructors of the data
type as functions "
     (mono "empty, vertex, overlay, connect")
     ". Another type class "
     (mono "ToGraph")
     " is for types that can be embedded in the "
     (mono "Graph")
     " data type by "
     (mono "toGraph :: ToGraph g => g -> Graph (ToVertex g)")
     ". These graph expressions may in turn be \"interpreted\" in fold
over the "
     (mono "Graph")
     " encoding, "
     (mono "foldg :: ToGraph g => r -> (ToVertex g -> r) -> (r -> r -> r) -> (r -> r -> r) -> g -> r")
     ". ")
    (*paragraph*
     "Working with "
     (mono "ToGraph")
     " is powerful, since algebraic graphs can converted between
representations, allowing code sharing between possibly disparate
types. In other words, the following is enough to run the graph
searches from above on any "
     (mono "ToGraph g")
     " type: ")
    ,foldg-tograph
    (*subsection* "More Information")
    (*paragraph*
     "There is much more to alga. The above hopefully gives an idea of
the library's algebra and its approach to graphs. Alga also provides
means for working with undirected graphs, relations, acyclic graphs,
bipartite graphs, and so on.")    
    (*subsection* "Previously")
    (*paragraph*
     ("The previous implementations in fact used ")
     (mono "ToGraph g")
     " to convert between representations to use the the "
     (*link* "Data.Graph" ,Data.Graph)
     " module, given a conversion from adjacency maps to the "
     (mono "Graph :: Array Int [Int]")
     " representation:"
     ,kl-snippet
     " The pipeline went something like: "
     (enum
      (item (mono "toAdjacencyMap . toGraph :: g -> AdjacencyMap (ToVertex g)") )
      (item (mono "fromAdjacencyMap :: AdjacencyMap (ToVertex g) -> GraphKL (ToVertex g)"))
      (item (mono "map (fmap fromVertexKL) . Data.Graph.dff . toGraphKL :: Data.Graph -> Forest (ToVertex g)"))))
    (*paragraph*
     (mono "Data.Graph")
     " provides good performance for the functions it does export,
since it works on memory-compact adjacency maps represented as "
     (mono "Array Int [Int]")
     " and is a mature module. Unfortunately, the array-based
representation does not play well with alga's algebraic graphs, so
it's not clear how reduce the number of conversions.")
    (*paragraph*
     "For more information about alga, its design, and its laws, I'd
suggest checking out any of the following: "
     " a series of "
     (*link* "blog posts" ,blog-post-link) ", "
     (*link* "this talk" ,video-link) ", "
     (*link* "this paper" ,algebraic-graphs-with-class) " or the "
     (*link* "documentation" ,algebraic-graphs)
     ".")))

(define depth-first-section
  `((*section* "Depth First Search")
    (*paragraph*
     "Depth first search is the simplest of the
implementations. Algorithmically, it's not too different from breadth
first search, but it's expressed more readily in haskell due to the
first class status of linked lists and the representation of forests
as free lists.")
    (*paragraph*
     "Here is the entire implementation (currently) found in alga:"
     ,dfsnippet)
    (*paragraph*
     "A depth first search forest is produced given a list of vertices
to search. Two mutually recursive functions drive the computation, "
     (mono "explore")
     " which checks that the next vertex is unexplored and if so passes it to "
     (mono "walk")
     ". "
     (mono "walk")
     " builds a tree from its argument, passing the neighboring
vertices back to "
     (mono "explore.")
     " To this end, sufficient state is a set of vertices that have
been visited. A bottom-up description:"
     (enum (item
	    (mono "adjacent v")
	    " exists because it's more pleasant to read than "
	    (mono "IntSet.toList (postIntSet v g)")
	    ".")
	   (item
	    (mono "discovered v")
	    " reports if the vertex has been discovered and marks it as
such if not.")
	   (item
	    (mono "walk v")
	    " includes the tree from "
	    (mono "v")
	    " in the forest by exploring "
	    (mono "v")
	    "'s neighbors.")
	   (item
	    (mono "explore vs")
	    " builds a forest from all undiscovered vertices in its
argument list.")
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
	    ".")))
    (*paragraph*
     (mono "dfsForestFrom\'")
     " has the tick because it is a function internal to the
module. The following three functions are the ones actually exported,
matching the original API from "
     (mono "Data.Graph") ":"
     ,dfs-api)))

(define topological-section
  `((*section* "Topological Sort")
    (*paragraph*
     "Given a directed graph, we return a valid topological ordering
of the vertices if it is acyclic, or we present a cycle otherwise. In
the case that the graph is acyclic, we produce the lexicographically
smallest such ordering (addressing an old "
     (*link* "issue" "https://github.com/snowleopard/alga/issues/2")
     "). The increased demands, relative to dfs, are reflected in
various ways in the implementation; more state is threaded, a hairier
monad is requested, and the compactness of dfs is gone. It's less
concise, but more interesting.")
    (*subsection* "Implementation")
    ,top-sort-core
    (*paragraph*
     "A topological ordering can be computed by sorting the vertices
by exit time during depth first search. To ensure the
lexicographically smallest such ordering, we procrastinate exploring
smaller vertices as long as possible: the "
     (mono "vertices")
     " are considered in descending order and "
     (mono "adjacent v")
     " is here defined as "
     (mono "toDescList $ postIntSet v g")
     ".")
    (*paragraph*
     "The search is characterized by working over state held in a record of type "
     (mono "S")
     " and by the ability to report a cycle. Whereas the dfs
implementation worked over "
     (mono "State IntSet a")
     " the monad here is "
     (mono "(MonadState S m, MonadCont m) => m a")
     ". The schemer in me was pleased to spot a decent opportunity for "
     (mono "callCC")
     ". When a cycle is discovered, the computation terminates
immediately and the wrapping/unwarpping of some part of the
computation in "
     (mono "Either")
     " is postponed until the very end.")
    (*paragraph*
     "The cycle reconstruction function is called when a back-edge is
encountered during the sort. It cons's parents to the cycle until the
ancestor that was Entered but not Exited is reached: "
     ,top-sort-cycle
     "The only notable aspect is the type for cycles, which was
informed by pesky compiler warnings and advice from Andrey Mokhov
about how to best get rid of them. He suggested that the way to avoid
incomplete pattern warnings was to find a better data structure--one
that couldn't represent impossible state. "
     (mono "NonEmpty")
     " it is!")
    (*subsection* "Other Details")
    (*paragraph*
     "The information here is uninteresting, but included for
thoroughness. Here are internal search state types:"
     ,top-sort-types
     "The state is represented as a record with three parts: parent
pointers stored in an "
     (mono "IntMap Int")
     ", node states as "
     (mono "Entered")
     " or "
     (mono "Exited")
     ", and a list of vertices ordered by exit time (the eventual
topological ordering). The interface to this state includes:"
     (enum
      (item
       (mono "nodeState")
       " to query if a node is unvisited, being processed, or
exited. The result of the query can be "
       (mono "Nothing")
       ", "
       (mono "Just Entered")
       ", or "
       (mono "Just Exited")
       " corresponding to the colors White, Gray, and Black in classic
algorithms books.")
      (item
       (mono "enter")
       " is called when visiting a vertex. The parent vertex and the
node state are updated.")
      (item
       (mono "enterRoot")
       " to explore a new component of the search tree. There is no
parent to enter, only the entry table is updated.")
      (item
       (mono "exit")
       " called when the given vertex's descendents have been processed."))
     ,top-sort-state
     "Vertices are visited once and exited once. An error is thrown when
this isn't the case (never).")))

(define benchmarks
  `((*section* "Benchmarks")
    "Here are links to various benchmarks done with criterion:"
    (*criterion-reports* "~/code/alga-bench")
    (*paragraph*
     "Benchmarks are run after construction, to focus on query
performance. fgl and containers don't seem to have easy support for
working with graphs whose vertices are not type Int, so most of the
benchmarks compare them against alga's AdjacencyIntMaps. Many of the
graphs are borrowed from "
     (*link* "haskell-perf/graphs." "https://github.com/haskell-perf/graphs"))
    (*paragraph*
     ,criterion-tables
     (*break*)
     "There are two benchmarks for topological sort,
since the graphs have cycles. At first blush, it seems like nonsense
to compare implementations because the new one short circuits once it
finds a cycle. On the other hand, the old alga implementation would
first run "
     (mono "topSort")
     " from Data.Graph and subsequently "
     (mono "guard $ isTopSortOf result")
     ". Thus, the ~6500 fold improvement over the old implementation
is a semi-legitimate comparison on directed cyclic graphs. On DAGs,
the improvement is ~3 fold. The graphs are made acyclic by removing
self-loops and reversing edges so that "
     (mono "(x,y) -> (min x y, max x y)")
     ". The vertices are then randomly permuted since the new topSort
implementation considers them in sorted order."
     ,real-world-graphs-summary     
     (*paragraph*
      "The source code for the benchmarks can be viewed at "
      (*link* "bench-alga" "https://github.com/jitwit/bench-alga/blob/master/report.hs")))))


(define alga-dfs-post
  `(html
    (head
     (*css*)
     (meta (@ (charset "UTF-8")))
     (title "DFS Alga"))
    (body
     (*post-title* "Graph searches in alga")
     ,@preamble
     ,@depth-first-section
     ,@topological-section
     ,@background
     ,@benchmarks)
    (*footer*)))

(define (render)
  (render-page alga-dfs-post "dfs-alga.html"))


