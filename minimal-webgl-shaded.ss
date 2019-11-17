(define page
  (let ((canvas-width "500")
        (canvas-height "500"))
    `(html
      (head
       (title "SHADED?")
       (meta (@ (charset "UTF-8")))
       (css)
       (js-script 0 "js/utils.js")
       (js-script 0 "js/minimal_webgl_shaded.js"))
      (body
       (section 1 "webgl?")
       (canvas (@ (id "canvas")
                  (width ,canvas-width)
                  (height ,canvas-height))))
      (footer))))
