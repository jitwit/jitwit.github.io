(define page
  (let ((canvas-width "800")
        (canvas-height "800"))
    `(html
      (head
       (title "mandelbrot")
       (meta (@ (charset "UTF-8")))
       (css-from 1)
       (js-script 1 "js/mandelbrot.js"))
      (body
       (section 1 "Mandelbrot")
       (canvas (@ (id "canvas")
                  (width ,canvas-width)
                  (height ,canvas-height))))
      (footer))))
