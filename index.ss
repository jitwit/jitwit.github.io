(load "style-sheet.ss")

(define index-page
  `(html
    (head
     (meta (@ (charset "UTF-8")))
     (title "jitwit"))
    (body
     (*link* "alga wip post" "dfs-alga.html"))))

(define (render)
  (render-page index-page "index.html"))
