
(define (map->globe theta phi)
  (* earth-radius
     (+ (* (cos theta) (up (cos phi) (sin phi) 0))
        (up 0 0 (sin theta)))))

(define dist-montreal-vancouver
  '(* earth-radius
      (acos (/ (dot-product (map->globe (degrees->radians 45.5) (degrees->radians -73.7))
                            (map->globe (degrees->radians 49.2) (degrees->radians -123.2)))
               (square earth-radius)))))

(define dist-montreal-kilimanjaro
  '(* earth-radius
      (acos (/ (dot-product (map->globe (degrees->radians 45.5) (degrees->radians -73.7))
                            (map->globe (degrees->radians -3) (degrees->radians 37.4)))
               (square earth-radius)))))

(define page
  `(html
    (head
     (css-from 1)
     (meta (@ (charset "UTF-8")))
     (title "Distance on a map"))
    (body
     (section 1 "Distance on a map")
     (p "This is a scribble using scmutils. The task is to calculate
the distance between two " (code "(lon,lat)") " points on the globe.")
     (section 2 "Scmutils")
     (p "A brief demonstration of how cool scmutils is.")
     (p "Let's define a fucntion to go from coordinates on a map to a point in "
        (code "R^3") ".")
     (scheme-define
      (define (map->globe theta phi)
        (* earth-radius
           (+ (* (cos theta) (up (cos phi) (sin phi) 0))
              (up 0 0 (sin theta))))))
     (p "We can punch in the north pole and get"
        (scheme-eval (map->globe pi/2 'phi)))
     (p "Or leave it all abstract"
        (scheme-eval (map->globe 'theta 'phi)))
     (p "Or how about montreal"
        (scheme-eval (map->globe (degrees->radians 45.5) (degrees->radians -73.5))))
     (section 2 "Distance")
     (p "So anyway, the distance between two points on the map can be found
with the angle between the vectors on the globe. That can be done with a dot product.")
     (p "Let's take Montreal to New York as the cow flies")
     (scheme-eval ,dist-montreal-vancouver)
     (p "A quick google search for the flight distance gave 3,680 km. Hooray.")
     (p "And how about Montreal to Mount Kilimanjaro")
     (scheme-eval ,dist-montreal-kilimanjaro)
     (p "Google says 11,862 km."))
    (footer)))

