(load "style-sheet.ss")

(define *title* "Expressing graph searches in haskell")

(define Content
  `((*title* ,*title*)
    (*paragraph*
     "Recently, I helped contribute to "
     (*link* "alga" "https://github.com/snowleopard/alga")
     ", a fun haskell library for working with graphs. The first
contribution was implementing breadth first search. The second took
ideas from the first to improve the implementations of depth first
search and topological sort.")
    (*section* "Backround on alga")
    (*paragraph*
     (*link* alga "https://github.com/snowleopard/alga")
     " is a fun haskell library for working with graphs. Graphs are
built form expressions using a handful of building blocks, after which
they may be instantiated to a suitable structure for further
processing (eg querying, modifying, searching, computing transitive or
symmetric closures, exporting to dot files, etc).
     "
     (br)
     "Here are the core blocks:"
     (*haskell* "data Graph a = Empty | Vertex a | Overlay (Graph a) (Graph a) | Connect (Graph a) (Graph a)")
     (tt "Empty")
     " denotes a graph with no vertices or edges and "
     (tt "Vertex a")
     " a graph with one vertex of type "
     (tt "a")
     " and no edges. More complex graphs are constructed by combining
smaller ones in two ways. "
     (tt "Overlay g h")
     " takes graphs " (tt "g") " and " (tt "h") 
     " to produce a graph whose vertex and edges sets are the unions
of g's and h's. Connect g h is similar, but additionally includes an
edge going from each vertex of g to each vertex of h."
     (br)
     "")))

(define alga-dfs-post
  `(html
    (head
     (meta (@ (charset "UTF-8")))
     (title "DFS Alga"))
    (body ,@Content)))

(define (run)
  (define file "alga.html")
  (when (file-exists? file)
    (delete-file file))
  (with-output-to-file file
    (lambda ()
      (put-html
       (render-post alga-dfs-post)))))
(run)
