(define-coordinates (up x y) R2-rect)
(define-coordinates (up r theta z) R3-cyl)

(define lemniscate 
  (- (square (+ (square x)
                (square y)))
     (* 2
        (square 'a)
        (- (square x) (square y)))))

(define part-a
  (simplify (lemniscate ((point R2-polar) (up 'r 'theta)))))

(define helix
  (- theta
     (* 'a z)))

(define part-b
  (simplify (helix ((point R3-rect) (up 'x 'y 'z)))))


(define page
  `(html
    (head
     (css-from 2)
     (meta (@ (charset "UTF-8")))
     (title "Exercise 2.1"))
    (body
     (section 2 "a) Lemniscate of Bernoulli")
     (scheme-eval part-a)
     (p "This simplifies to "
        (scheme-eval '(- (expt r 2) (* 2 (expt a 2) (cos (* 2 theta)))))
        " with some help from pen and paper.")
     (section 2 "b) Helix")
     (p "Take the helix to have constant "
        (code "R")
        " and naked "
        (code "theta")
        " and scaled "
        (code "z") "."
        " Then "
        (code "theta = a*z")
        " which gives: "
        (scheme-eval part-b)))))
