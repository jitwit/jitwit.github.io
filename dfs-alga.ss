(load "style-sheet.ss")
(load "dfs-alga-code-snippets.ss")

(define *title* "Expressing graph searches in haskell")
(define snowleopard/alga "https://github.com/snowleopard/alga")
(define blog-post-link "https://github.com/snowleopard/alga#blog-posts")
(define video-link "https://www.youtube.com/watch?v=EdQGLewU-8k")
(define Data.Graph "http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Graph.html")
(define algebraic-graphs "http://hackage.haskell.org/package/algebraic-graphs")
(define containers "http://hackage.haskell.org/package/containers-0.6.2.1")

(define introduction
  `((*section* "Background on alga")
    (*paragraph*
     "With alga, graphs are constructed using a handful of building
blocks---from empty graphs, vertices, overlays, and
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
     "There is of course much more to alga. The above merely gives an
idea of the library's algebra, the expression-oriented approach to
construction of graphs, and the interface used for processing
them. Alga also provides means for working with undirected graphs,
relations, acyclic graphs, bipartite graphs, and so on."
     " More information about alga, its design, and its laws may be
found in its "
     (*link* "documentation" ,algebraic-graphs)
     ", a series of "
     (*link* "blog posts" ,blog-post-link) ", and in "
     (*link* "this talk" ,video-link) ".")))

(define depth-first-section
  `((*section* "Depth First Search")
    "Now the new implementation."
    (*sub-section* "Previously")
    "There is a lot of power that comes from using the "
    (*link* "Data.Graph" ,Data.Graph)
    " to take advantage of existing the dfs implementation."
    ,dfs-implementation
    ))

(define the-post
  `((*post-title* ,*title*)
    (*paragraph*
     "Recently, I helped contribute to "
     (*link* "alga" ,snowleopard/alga)
     ", a fun haskell library for working with graphs developed by
Andrey Mokhov. My first contribution implemented breadth first
search. The second took ideas from the first to improve the existing
implementations of depth first search and topological sort. This post
describes the implementations and how haskell lends itself well to the
expression of these classic algorithms.")
    ,@introduction
    ,@depth-first-section
    (*section* "Topological Sort")
    ,top-sort-implementation))

(define alga-dfs-post
  `(html
    (head
     (*haskell-css*)
     (meta (@ (charset "UTF-8")))
     (title "DFS Alga"))
    (body ,@the-post)))

(render-page alga-dfs-post "dfs-alga.html")

