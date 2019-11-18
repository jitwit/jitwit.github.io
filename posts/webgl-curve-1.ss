(define page
  (let ((canvas-width "800")
        (canvas-height "800"))
    `(html
      (head
       (title "Curve?")
       (meta (@ (charset "UTF-8")))
       (css-from 1)
       (js-script 1 "js/webgl_curve_1.js"))
      (body
       (section 1 "graph?")
       (canvas (@ (id "canvas")
                  (width ,canvas-width)
                  (height ,canvas-height))))
      (footer))))
