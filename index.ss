(load "style-sheet.ss")

(define index-page
  `(html
    (head
     (meta (@ (charset "UTF-8")))
     (title "jitwit"))
    (body
     )))

(define (render)
  (render-page index-page "index.html"))
