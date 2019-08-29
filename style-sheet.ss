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

(define hs-css
  `(style "
.hs-keyword { color: sienna; }
.hs-conid { color: teal; } 
.hs-conop { color: teal; } 
.hs-keyglyph { color: steelblue; }
.hs-layout { color: steelblue; }
.hs-definition { color: darkslateblue; }
.hs-varid { color: darkslateblue; }
.hs-varop { color: crimson; }
               "))

(define (code-class class snippet)
  `(span (@ (class ,class))
	 ,snippet))

(define (hs-keyword keyword)
  (code-class "hs-keyword" keyword))

(define (hs-conid conid)
  (code-class "hs-conid" conid))

(define (hs-varid varid)
  (code-class "hs-varid" varid))

(define style-sheet
  `((*haskell* . ,(lambda (tag str)
		    (colorize-haskell str)))
    (mono . ,(lambda (_ str)
	       `(tt ,str)))
    (*break* . ,(lambda _ '(br)))
    (*local-css* . ,(lambda _
		      hs-css))
    (*title* . ,(lambda (_ title)
		  `(h2 ,title)))
    (*section* . ,(lambda (_ title)
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
