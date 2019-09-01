(load "style-sheet.ss")

(define index-page
  `(html
    (head
     (*css*)
     (meta (@ (charset "UTF-8")))
     (title "jitwit"))
    (body
     (*section* "Posts")
     (*link* "Expressing graph searches in haskell" "dfs-alga.html"))
    (*footer*)))

(define (render)
  (render-page index-page "index.html"))
