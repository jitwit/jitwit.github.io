(load "style-sheet.ss")

(define index-page
  `(html
    (head
     (*css*)
     (meta (@ (charset "UTF-8")))
     (title "jitwit"))
    (body
     (*section* "Posts"))
    (*footer*)))
;;     (*link* "Graph searches in alga" "dfs-alga.html")

(define (render)
  (render-page index-page "index.html"))
