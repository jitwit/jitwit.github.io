(import (sxml-mini))

;; todo make reports for csv results  
(define (include-criterion project-dir)
  (let ((reports (filter (lambda (file)
			   (string=? (path-extension file) "html"))
			 (directory-list project-dir))))
    (for-each (lambda (report)
		(system
		  (format "cp ~a ~a"
			  (format "~a/~a" project-dir report)
			  (format "criterion/~a" report))))
	      reports)
    reports))

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
    (item . ,(lambda (_ . stmts)
	       `(li ,@stmts)
	       ))
    (enum . ,(lambda (_ . items)
	       `(ul ,@items)))
    (*break* . ,(lambda _ '(br)))
    (*haskell-css* . ,(lambda _
			'(link (@ (rel "stylesheet")
				  (type "text/css")
				  (href "css/haskell.css")))))
    (*css* . ,(lambda _
		'(link (@ (rel "stylesheet")
			  (type "text/css")
			  (href "css/style.css")))))
    (*post-title* . ,(lambda (_ title)
		       `(h1 ,title)))
    (*section* . ,(lambda (_ title)
		    `(h2 ,title)))
    (*subsection* . ,(lambda (_ title)
		       `(h3 ,title)))
    (*paragraph* . ,(lambda (_ . nodes)
		      `(p (section ,@nodes))))
    ;; fetch .html files assumed to be criterion reports from the
    ;; given directory.
    (*criterion-reports* . ,(lambda (_ project)
			      `(ul ,(map (lambda (report)
					   `(li (a (@ (href ,(format "criterion/~a" report)))
						   ,(path-root report))))
					 (include-criterion project)))))
    (*link* . ,(lambda (_ link href)
		 `(a (@ (href ,href))
		     ,link)))
    (*footer* . ,(lambda x
		   (let* ((now (current-date))
			  (build (format "updated: ~a/~a/~a"
					 (date-day now)
					 (date-month now)
					 (date-year now))))
		     `(footer
		       ,build))))
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
