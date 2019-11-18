(define page
  (let ((canvas-width "500")
        (canvas-height "500"))
    `(html
      (head
       (title "SHADED?")
       (meta (@ (charset "UTF-8")))
       (css-from 1)
       (js-script 1 "js/minimal_webgl_shaded.js"))
      (body
       (section 1 "webgl?")
       (canvas (@ (id "canvas")
                  (width ,canvas-width)
                  (height ,canvas-height))))
      (footer))))
