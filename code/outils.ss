(define (render-html post)
  (pre-post-order post style-sheet))

(define (render-page page file)
  (when (file-exists? file)
    (delete-file file))
  (with-output-to-file file
    (lambda ()
      (put-html
       (render-html page)))))

(define (quick-render page)
  (let ((tmp "tmp.html"))
    (render-page page tmp)
    (system (string-append "open " tmp))
    (delete-file tmp)))

(define (intersperse sep xs)
  (let ((xs (fold-right (lambda (x y)
                          (cons* sep x y))
                        '()
                        xs)))
    (if (null? (cdr xs))
        '()
        (cdr xs))))
