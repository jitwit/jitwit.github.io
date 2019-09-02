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
     ", a very cool haskell library for working with graphs developed
by Andrey Mokhov. My first contribution implemented breadth first
search. The second took ideas from the first to improve the existing
implementations of depth first search and topological sort. This post
describes the implementations and illustrates how haskell lends itself
well to the expression of these classic algorithms.")
    (*paragraph*
     "The first few sections dive in to the
implementations. Background information about alga will follow
containing pointers to better introductions. The final section
discusses the previous implementations and includes benchmarks.")
    (*paragraph*
     "For now it suffices to know that one of the main representations
alga uses for directed graphs is adjacency maps using data structures
from the containers package, "
     (mono "Map a (Set a)")
     " for graphs with vertices that have "
     (mono "Ord a")
     " instances or "
     (mono "IntMap IntSet")
     " for ones with "
     (mono "Int")
     " vertices. These are the representations the implementations are concerned with.")))

(define background
  `((*section* "Alga Background")
    "A brief introduction to alga, explaining where the
implementations fit in."
    (*subsection* "Core Idea")
    (*paragraph*
     "With alga, graphs are constructed algebraically. A (di-)graph is
some object with a set of vertices and a set edges, ordered pairs of
vertices. Alga expresses this by "
     ,core-data-type
     "The vertices are drawn from elements of some type "
     (mono "a")
     " and wrapped by the "
     (mono "Vertex")
     " constructor. This construction is algebraic in the sense that
there are two closed operations for building graphs from other
graphs, "
     (mono "Overlay")
     " and "
     (mono "Connect")
     ". These operations satisfy additional axioms analogous to + and
* (see the links below for proper details). ")
    (*paragraph*
     (mono "Overlay g h")
     " is the graph whose vertex and edge sets are the unions of "
     (mono "g")
     "'s and "
     (mono "h")
     "'s. "
     (mono "Connect g h")
     " is the graph whose vertex and edge sets contain those from
overlay, as well as an edge from every vertex of "
     (mono "g")
     " to every vertex of "
     (mono "h") ".")
    (*subsection* "Construction as a DSL")
    (*paragraph*
     "The "
     (mono "Graph")
     " data type can be seen as a DSL for graph construction. In fact,
it corresponds to an initial encoding of the language. Alga also
supplies a final encoding with the type class "
     (mono "Graph g")
     ", which requires analogues for the type constructors of the data
type specified as functions "
     (mono "empty, vertex, overlay, connect")
     " for membership. There is also a type class "
     (mono "ToGraph")
     " for types that can be converted to the "
     (mono "Graph")
     " ADT by way of "
     (mono "toGraph :: ToGraph g => g -> Graph (ToVertex g)")
     ". These graph expressions are in turn \"interpreted\" by way of
some fold over the "
     (mono "Graph")
     " ADT, "
     (mono "foldg :: ToGraph g => r -> (ToVertex g -> r) -> (r -> r -> r) -> (r -> r -> r) -> g -> r")
     ". ")
    (*paragraph*
     "This is powerful, because under this framework, algebraic graphs
can marshalled between representations, offering code sharing between
seemingly disparate types. In other words, the following is enough to
run the graph searches from above on any "
     (mono "ToGraph g")
     " type: ")
    ,foldg-tograph
    (*subsection* "More Information")
    (*paragraph*
     "Naturally, there is much more to alga. The above hopefully gives
an idea of the library's algebra, the language-oriented approach to
graphs, and the interface used for processing them. Alga also provides
means for working with undirected graphs, relations, acyclic graphs,
bipartite graphs, and so on."
     " For more detailed information about alga, its design, and its
laws, I'd suggest checking out any of the following: "
     " a series of "
     (*link* "blog posts" ,blog-post-link) ", "
     (*link* "this talk" ,video-link) ", "
     (*link* "this paper" ,algebraic-graphs-with-class) " or the "
     (*link* "documentation" ,algebraic-graphs)
     ".")
    (*subsection* "Previously")
    (*paragraph*
     ("Given a type with a ")
     (mono "ToGraph g")
     " instance, we can use code from "
     (*link* "Data.Graph" ,Data.Graph)
     " simply by defining a conversion form AdjacencyMaps to the Graph type defined there.
The glue to to do so: "
     ,kl-snippet)
    (*paragraph*
     (mono "Data.Graph")
     " provides good performance for the functions it does export,
since it works on memory-compact adjacency maps represented as "
     (mono "Array Int [Int]")
     ". Unfortunately, the array-based representation does not play
well with alga's algebraic graphs, so it's not possible to skip this
conversion step. For arbitrary "
     (mono "ToGraph g")
     "'s the result is three passes converting representations: "
     (enum
      (item (mono "g") " -> " (mono "AdjacencyMap a"))
      (item (mono "AdjacencyMap a") " -> " (mono "GraphKL a"))
      (item (mono "Forest Int") " -> " (mono "Forest a")
	    "  (in the case of " (mono "dfs") ")")))
    "The above describes the previous approach to implementing dfs in
alga. It's a testament to the flexibility offered by the DSL approach
to library design. So, the new dfs implementation is basically a
response to the question, would it be better to avoid conversion by
operating on adjacency maps directly? The question is answered
affirmatively, by benchmarking!"))

(define depth-first-section
  `((*section* "Depth First Search")
    (*paragraph*
     "Depth first search is the simplest of the
implementations. Algorithmically, it's not very different from breadth
first search, but is expressed more readily in haskell due to the
first class status of linked lists and the representation of forests
as free lists. ")
    (*paragraph*
     "Here is the whole implementation (currently) found in alga:"
     ,dfsnippet)
    (*paragraph*
     "A depth first search forest is produced given a list of vertices
to search. Two mutually recursive functions drive the computation, "
     (mono "explore")
     " which checks that the next vertex is unexplored and if so passes it to "
     (mono "walk")
     ". "
     (mono "walk")
     " builds a tree from its argument, passing neighboring vertices back to "
     (mono "explore.")
     " To this end, the necessary (and sufficient) state is keeping
track of which vertices have been visited. A bottom-up description:"
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
	    " builds a forest from all undiscovered vertices in "
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
     "The goals for implementing topological sort are more complex
than for dfs. Given a directed graph, we return either a valid
topological ordering of the vertices or a cycle. If there the graph is
indeed acyclic, we want to produce the lexicographically smallest such
ordering (addressing an old "
     (*link* "issue" "https://github.com/snowleopard/alga/issues/2")
     ").")
    (*paragraph*
     "These increased demands are reflected in various ways in the
implementation; more state is threaded, a hairier monad is requested,
and the compactness of dfs is gone. It's less concise, but more
interesting. Unlike the dfs implementaiton, I'll split up the
presentation for clarity.")
    
    (*subsection* "Implementation")
    ,top-sort-core
    (*paragraph*
     "A topological ordering can be computed by sorting the vertices
by exit time during depth first search. So, to ensure the
lexicographically smallest ordering, we procrastinate exploring
smaller vertices as long as possible. "
     "The graphs "
     (mono "vertices")
     " are considered in descending order and "
     (mono "adjacent v")
     " is defined as "
     (mono "toDescList . flip postIntSet g"))
    (*paragraph*
     "I find it satisfying that this implementation comes close (in my
mind at least) to the pseudocode from the classic CLSR algorithms
book. There, top sort is specified as:"
     (enum
      (mono "Topological-sort(G):")
      (item
       "call "
       (mono "DFS(G)")
       " to compute finishing times for each vertex"
       (*break*)
       "(the for loop in DFS is exactly "
       (mono "forM_ vertices dfsRoot")
       " and "
       (mono "DFS-VISIT(G,v)")
       " corresponds to "
       (mono "dfs")
       " in the let clause).")
      (item
       "as each vertex is finished, insert in onto the front of a linked list"
       (*break*)
       "(precisely what "
       (mono "exit")
       " does).")
      (item
       "Return the linked list"
       (*break*)
       "("(mono "Right <$> gets order") ").")))

    (*subsection* "Monad")
    (*paragraph*
     "The search is characterized by working over state held in a record of type "
     (mono "S")
     " and by the ability to report a cycle. Whereas the dfs
implementation worked over "
     (mono "State IntSet a")
     " the monad here is "
     (mono "(MonadState S m, MonadCont m) => m a")
     ".")
    (*paragraph*
     "The schemer in me was pleased to spot a decent opportunity for "
     (mono "callCC")
     ". If a cycle is discovered, it allows the computation to
terminate immediately and it also avoids the wrapping/unwarpping of
some part of the computation in "
     (mono "Either")
     " until the very end.")
    (*subsection* "Cycles")
    (*paragraph*
     "The cycle construction function is called when a back-edge is
encountered during the sort. It cons's parents to the cycle until the
ancestor that was Entered but not Exited is reached."
     ,top-sort-cycle
     "The remarkable aspect is the type of cycles, which was informed
by pesky compiler warnings and advice from Andrey Mokhov about how to
best get rid of them. He suggested way to avoid incomplete pattern
warnings was to find a better data structure--one that couldn't
represent impossible state. "
     (mono "NonEmpty")
     " it is!")
    (*subsection* "State")
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
       " to query if a node is unvisited, being processed, or exited.")
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
     "The benchmarks are run after construction, to focus on measuring
queries. fgl and containers don't seem to have easy support for
working with graphs whose vertices are not type Int, so most of the
benchmarks compare them against alga's AdjacencyIntMaps. There are
benches comparing the new and old alga implementations for
AdjacencyMaps as well. Many of the graphs are taken from "
     (*link* "haskell-perf/graphs" "https://github.com/haskell-perf/graphs")
     ". The words graph is Knuth's graph formed by 5 letter english
words, where two words are connected if they differ by one
character.")
    (*paragraph*
     "The source code for the benchmarks can be viewed at "
     (*link* "report.hs" "https://github.com/jitwit/bench-alga/blob/master/report.hs"))))

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
     ,@depth-first-section
     ,@topological-section
     ,@background
     ,@benchmarks)
    (*footer*)))

(define (render)
  (render-page alga-dfs-post "dfs-alga.html"))

;;(render)

