(define (mathml expr)
  (match expr
    (((? math-function? f) x)
     `(,(math-function f) ,(parenthesize x)))
    (((? math-operator? op) x ...)
     (intersperse (math-operator op) (map mathml x)))
    (('expt x n)
     `(msup (mrow ,(mathml x))
            (mrow ,(mathml n))))
    ((? math-number? expr) `(mn ,(number->string expr)))
    ((? math-identifier? expr) (math-identifier expr))))

(define (math-number? n)
  (number? n))

(define (math-identifier? x)
  (symbol? x))

(define (math-identifier x)
  (match x
    ((? greek? x) `(mi ,(greek-identifier x)))
    (else `(mi ,(symbol->string x)))))

(define (greek? x)
  (case x
    ((omega theta phi Omega Theta Phi) #t)
    (else #f))
  #f)

(define (greek-identifier x)
  (case x
    ((phi) '(*text* "&phi;"))
    ((omega) '(*text* "&omega;"))
    (else (error 'greek-identifier "idk"))))

(define (parenthesize expr)
  `((mo "(") ,(mathml expr) (mo ")")))

(define (math-operator? op)
  (memq op '(+ *)))

(define (math-operator op)
  (match op
    ('+ `(mo "+"))
    ('* `(mo "*"))))

(define (math-function? f)
  (memq f '(cos sin tan acos asin atan)))

(define (math-function f)
  (match f
    ('cos `(mi "cos"))
    ('sin `(mi "sin"))
    ('tan `(mi "tan"))
    ('acos `(msup (mi "cos") (mn "-1")))
    ('asin `(msup (mi "sin") (mn "-1")))
    ('atan `(msup (mi "tan") (mn "-1")))))

(define (scheme->mathml expr)
  (let ((expr (simplify expr)))
    `(math (@ (mathbackground "#EEEEEE"))
           (mrow ,(mathml expr)))))
