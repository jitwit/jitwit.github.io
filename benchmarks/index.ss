
(define page
  `(html
    (head
     (css-from 1)
     (meta (@ (charset "UTF-8")))
     (title "Benchmarks"))
    (body
     (section 1 "Benchmarks")
     (section 2 "alga")
     "Various criterion reports benchmarking my contributions to alga."
     (ul
      (li
       (link "SCC haskperf" "criterion/scc-bench.html")
       " compares the former \"KL\" graph scc to the wip AM and AIM implementations on the real world graphs from haskperf.")
      (li
       (link "SCC social" "criterion/rw-scc-bench.html")
       " same as above but with a few social network graphs from SNAP.")
      (li (link "topological sort" "criterion/topological-bench.html"))
      (li (link "depth first" "criterion/depth-first-bench.html"))
      (li (link "breadth first" "criterion/breadth-first-bench.html"))))
    (footer)))
