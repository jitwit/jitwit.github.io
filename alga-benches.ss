(load "style-sheet.ss")

(define alga-benches
  `(html
    (head
     (*css*)
     (meta (@ (charset "UTF-8")))
     (title "Links to alga benchmarks"))
    (body
     (*link* "\"real-world\" scc" "https://jitwit.github.io/criterion/rw-scc-bench.html")
     (*link* "haskperf scc" "https://jitwit.github.io/criterion/scc-bench.html"))
    (*footer*)))

(define (render)
  (render-page alga-benches "alga-benches.html"))
