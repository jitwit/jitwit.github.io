(import (sxml-mini)
        (matchable)
        (scmutils base)
        (scmutils generic)
        (scmutils calculus))

(define src-files
  '("code/outils.ss"
    "code/mathml.ss"
    "code/style-sheet.ss"))

(for-all load src-files)

(define (make-page page.ss)
  ;;; each *.ss file that's a should defines variable page to be its
  ;;; content...
  (load page.ss)
  (render-page page (string-append (path-root page.ss) ".html")))
