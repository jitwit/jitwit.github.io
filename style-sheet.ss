(import (chez sxml-mini))

;; plan: write haskell string to some temp file, call HsColor -html -partial on it,
;; use process to read the system output directly

(define (colorize-haskell haskell-string)
  (let* ((tmp (format "/tmp/~a"
  		      (gensym->unique-string (gensym))))
  	 (hscolor (format "HsColour -css -partial < ~a" tmp)))
    (with-output-to-file tmp
      (lambda ()
  	(display haskell-string)))
    (let* ((ports (process hscolor))
	   (out (car ports))
	   (in (cadr ports)))
      (let loop ((x (read-char out)) (result '()))
	(if (eof-object? x)
	    (begin
	      (close-input-port out)
	      (close-output-port in)
	      (delete-file tmp)
	      `(*raw-html* ,(list->string (reverse result))))
	    (loop (read-char out) (cons x result)))))))

(define style-sheet
  `((*haskell* . ,(lambda (tag str)
		    (colorize-haskell str)))
    (mono . ,(lambda (_ str)
	       `(tt ,str)))
    (*break* . ,(lambda _ '(br)))
    (*haskell-css* . ,(lambda _
			'(link (@ (rel "stylesheet")
				  (type "text/css")
				  (href "css/haskell.css")))))
    (*post-title* . ,(lambda (_ title)
		       `(h1 ,title)))
    (*section* . ,(lambda (_ title)
		    `(h2 ,title)))
    (*sub-section* . ,(lambda (_ title)
			`(h3 ,title)))
    (*paragraph* . ,(lambda (_ . nodes)
		      `(p (section ,@nodes))))
    (*link* . ,(lambda (_ link href)
		 `(a (@ (href ,href))
		     ,link)))
    (*default* . ,(lambda x x))
    (*text* . ,(lambda (tag str) str))))

(define (render-html post)
  (pre-post-order post style-sheet))

(define (render-page page file)
  (when (file-exists? file)
    (delete-file file))
  (with-output-to-file file
    (lambda ()
      (put-html
       (render-html page)))))
