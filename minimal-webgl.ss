(define page
  (let ((canvas-width "500")
        (canvas-height "500"))
    `(html
      (head
       (title "WEBGL?")
       (meta (@ (charset "UTF-8")))
       (css)
       (js-script 0 "js/minimal_webgl.js"))
      (body
       (section 1 "ok?")
       (canvas (@ (id "canvas")
                  (width ,canvas-width)
                  (height ,canvas-height))))
      (footer))))
