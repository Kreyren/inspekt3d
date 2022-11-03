#| rocking-horse.scm

Copyright 2018 Kavalogic, Inc.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

  (1) Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  (2) Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in
  the documentation and/or other materials provided with the
  distribution.

  (3) The name of the author may not be used to
  endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

|#

(use-modules (inspekt3d viewer)
             (libfive shapes) (libfive kernel) (libfive vec)
             (libfive transforms) (libfive csg))

(define (make-horse-shape)
    (let* (;;
           ;; Body
           ;;
           (body-length 1)
           (body-width (* 1/2 body-length))
           (body-thickness (* 1/12 body-length))
           (body-cutout-circle-radius (/ body-length 2))
           (body-cutout-circle (circle body-cutout-circle-radius
                                       #[(/ body-length 2) 0]))
           (body-cutout-circle-offset (+ body-cutout-circle-radius
                                         (/ body-width 4)))
           (body-rounding-factor 1/8)
           (body (rotate-x
                  (extrude-z
                   (difference
                    (rounded-rectangle #[0 (- (/ body-width 2))]
                                       #[body-length (/ body-width 2)]
                                       body-rounding-factor)
                    (move body-cutout-circle
                          #[0 body-cutout-circle-offset 0])
                    (move body-cutout-circle
                          #[0 (- body-cutout-circle-offset)]))
                   (- (/ body-thickness 2))
                   (/ body-thickness 2))
                  (/ pi 2)))
           ;;
           ;; Legs
           ;;
           (leg-length (* 2/3 body-length))
           (leg-radius (* 3/100 body-length))
           (leg-angle (/ pi 8))
           (leg (cylinder-z leg-radius leg-length))
           (leg-offset-x (+ leg-radius 1/10))
           (leg-offset-y (- (/ body-width 2) leg-radius 1/10))
           ;;
           (legs (rotate-x
                  (union
                   (move (rotate-x leg leg-angle)
                         #[leg-offset-x (- leg-offset-y) 0])
                   (move (rotate-x leg (- leg-angle))
                         #[leg-offset-x leg-offset-y 0])
                   (move (rotate-x leg leg-angle)
                         #[(- body-length leg-offset-x) (- leg-offset-y) 0])
                   (move (rotate-x leg (- leg-angle))
                         #[(- body-length leg-offset-x) leg-offset-y 0]))
                  (/ pi 2)))
           ;;
           ;; Rockers
           ;;
           (rocker-radius (* 5/2 body-length))
           (rocker-length (* body-length 8/5))
           (rocker-width (* 3 leg-radius))
           (rocker-thickness (* 3 leg-radius))
           (rocker-half-angle
            (asin (/ (/ rocker-length 2) rocker-radius)))
           (outer-rocker-circle (circle rocker-radius))
           (inner-rocker-circle (circle (- rocker-radius rocker-width)))
           (rocker-intersection-triangle
            (let* ((iso-side-length (* 2 rocker-radius))
                   (offset-x (* iso-side-length (sin rocker-half-angle)))
                   (offset-y (- (* iso-side-length (cos rocker-half-angle)))))
              (triangle #[0 0] #[(- offset-x) offset-y] #[offset-x offset-y])))
           (rocker-end-radius (* (/ rocker-width 2) 5/4))
           (rocker-end-offset-y (+ (- rocker-radius)
                                   (- rocker-width rocker-end-radius)))
           (rocker-end (circle rocker-end-radius #[0 rocker-end-offset-y]))
           (right-rocker-end (rotate-z rocker-end rocker-half-angle))
           (left-rocker-end (rotate-z rocker-end (- rocker-half-angle)))
           (rocker-profile (union (intersection
                                   (difference outer-rocker-circle
                                               inner-rocker-circle)
                                   rocker-intersection-triangle)
                                  left-rocker-end
                                  right-rocker-end))
           (rocker-offset-x (/ body-length 2))
           (rocker-offset-y (+ (* leg-length
                                  (cos leg-angle)) ; adjust for leg angle
                               (/ (- rocker-radius ; adjust for rocker "curl"
                                     (* rocker-radius
                                        (cos rocker-half-angle))) 2)
                               (/ rocker-width 2))) ;adjust for rocker width
           (rocker-offset-z (+ (* leg-length (sin leg-angle))
                               leg-offset-y)) ; legs rotated, use y offset
           ;;
           (rocker (move (extrude-z rocker-profile
                                    (- (/ rocker-thickness 2))
                                    (/ rocker-thickness 2))
                         #[rocker-offset-x
                           (- rocker-radius rocker-offset-y)
                           0]))
           (left-rocker (move rocker #[0 0 (- rocker-offset-z)]))
           (right-rocker (move rocker #[0 0 rocker-offset-z]))

           (rocker-rod-radius (/ leg-radius 3/2))
           (rocker-rod-offset-z (+ rocker-offset-z (/ rocker-thickness 4)))
           (rocker-rod (cylinder-z rocker-rod-radius
                                   (* 2 rocker-rod-offset-z)
                                   #[0
                                     rocker-end-offset-y
                                     (- rocker-rod-offset-z)]))
           (front-rocker-rod (move (rotate-z rocker-rod rocker-half-angle)
                                   #[rocker-offset-x
                                   (- rocker-radius rocker-offset-y)
                                   0]))
           (back-rocker-rod (move (rotate-z rocker-rod (- rocker-half-angle))
                                  #[rocker-offset-x
                                  (- rocker-radius rocker-offset-y)
                                  0]))
           ;;
           (rocker-support-length (* 3 leg-radius))
           (rocker-support-width (* (/ rocker-width 2) 11/10))
           (rocker-support (box #[(- (/ rocker-support-length 2))
                                (- (/ rocker-support-width 2))
                                (- (+ rocker-offset-z (/ rocker-thickness 2)))]
                                #[(/ rocker-support-length 2)
                                (/ rocker-support-width 2)
                                (+ rocker-offset-z (/ rocker-thickness 2))]))
           ;;
           (rocker-support-offset-y (+ (- rocker-offset-y)
                                       rocker-thickness
                                       1/50))
           ;;
           (front-rocker-support (move rocker-support
                                       #[(- body-length leg-offset-x)
                                       rocker-support-offset-y
                                       0]))
           (back-rocker-support (move rocker-support
                                      #[leg-offset-x
                                      rocker-support-offset-y
                                      0]))
           ;;
           (rocker-assembly (union right-rocker
                                   left-rocker
                                   front-rocker-support
                                   back-rocker-support
                                   front-rocker-rod
                                   back-rocker-rod))
           ;;
           ;; Neck
           ;;
           (neck-length (* 7/24 body-length))
           (neck-width (* 1/2 body-length))
           (neck-angle (/ pi 10))
           (neck-thickness (* 2/3 body-thickness))
           ;;            shear-x-y shape #[x0 y0] height offset [base-offset]
           (neck-profile (shear-x-y
                          ;; taper-x-y shape #[x0 y0] height scale [base-scale]
                          (taper-x-y
                           (rectangle #[(- (/ neck-length 2))
                                      (- (/ neck-width 2))]
                                      #[(/ neck-length 2)
                                      (/ neck-width 2)])
                           #[0 (- (/ neck-width 2))] neck-width 1/2 1)
                          #[0 (- (/ neck-width 2))] neck-width
                          (* neck-length (/ neck-angle (/ pi 4))) 0))
           (neck (move
                  (extrude-z neck-profile
                             (- (/ neck-thickness 2))
                             (/ neck-thickness 2))
                  #[(- body-length (/ neck-length 2))
                  (+ (/ neck-width 2) (/ body-thickness 4))
                  0]))
           ;;
           ;; Head
           ;;
           (head-circle-radius (* 1/8 body-length))
           (nose-circle-radius (/ head-circle-radius 2))
           (muzzle-length (* 6/5 neck-length))
           (head-thickness (* 1 body-thickness))
           (head-angle (- (/ pi 8)))
           (bridle-hole-radius (/ leg-radius 2))
           ;;
           (head-circle (circle head-circle-radius))
           (nose-circle (circle nose-circle-radius #[0 muzzle-length]))
           (bridle-hole-circle (circle bridle-hole-radius #[0 muzzle-length]))
           ;;       taper-x-y shape #[x0 y0] height scale [base-scale]
           (muzzle (taper-x-y
                    (rectangle #[(- head-circle-radius) 0]
                               #[head-circle-radius muzzle-length])
                    #[0 0] muzzle-length
                    (/ nose-circle-radius head-circle-radius) 1))
           ;;
           (head-profile (difference (union head-circle
                                            muzzle
                                            nose-circle)
                                     bridle-hole-circle))
           ;;
           (head-offset-x (- body-length 1/100))
           (head-main (move
                       (rotate-z
                        (extrude-z head-profile
                                   (- (/ head-thickness 2))
                                   (/ head-thickness 2))
                        (+ (- (/ pi 2)) head-angle))
                       #[head-offset-x neck-width 0]))
           ;;
           (head-bar-height (* 7/8 body-width))
           (head-bar-radius (/ leg-radius 3/2))
           ;;
           (head-bar (cylinder-z head-bar-radius
                                 head-bar-height
                                 #[head-offset-x
                                   neck-width
                                   (- (/ head-bar-height 2))]))
           ;;
           (ear-length (* 1/4 head-circle-radius))
           (ear-thickness (* 3/8 head-thickness))
           (ear-cylinder-radius (* 7/8 head-circle-radius))
           (ear-cylinder (cylinder-z ear-cylinder-radius
                                     ear-thickness
                                     #[0 0 (- (/ ear-thickness 2))]))
           (ear (intersection
                 (move ear-cylinder
                       #[(- ear-length ear-cylinder-radius) 0 0])
                 (move ear-cylinder
                       #[(- ear-cylinder-radius ear-length) 0 0])))
           (ear-offset-x (+ head-offset-x (- (/ head-circle-radius 8))))
           (ear-offset-y (+ neck-width head-circle-radius))
           (ear-offset-z (/ head-thickness 2))
           (right-ear (move ear #[ear-offset-x ear-offset-y ear-offset-z]))
           (left-ear (move ear #[ear-offset-x ear-offset-y (- ear-offset-z)]))
           ;;
           (head-assembly (union head-main
                                 left-ear
                                 right-ear
                                 head-bar))
           ;;
           ;; Tail
           ;;
           (tail-radius (* body-length 5/16))
           (tail-width (* tail-radius 5/12))
           (tail-thickness neck-thickness)
           ;;
           (tail-cylinder (cylinder-z tail-radius
                                      tail-thickness
                                      #[0 0 (- (/ tail-thickness 2))]))
           ;;
           (tail-box-square-dimension (* tail-radius 11/10))
           (tail-box-thickness (* tail-thickness 11/10))
           ;;
           (tail-box (box #[(- tail-box-square-dimension)
                          (- tail-box-square-dimension)
                          (- (/ tail-box-thickness 2))]
                          #[(- tail-radius tail-width)
                          tail-box-square-dimension
                          (/ tail-box-thickness 2)]))
           ;;
           (tail-base-x (- tail-radius tail-width))
           (tail-base-y (* tail-radius (sin
                                        (acos (/ tail-base-x tail-radius)))))
           (tail-upright (move (difference tail-cylinder tail-box)
                               #[(- tail-base-x)
                               (- tail-base-y)
                               0]))
           ;;
           (tail-angle (/ pi 4))
           (tail (move
                  (rotate-z tail-upright (- tail-angle))
                  #[2/100 (/ body-thickness 2) 0]))
           ;;-;;          ;;-;;
           ;;                ;;
           ;; Complete horse ;;
           ;;                ;;
           ;;-;;          ;;-;;
           (rocking-horse
            (let* ((model
                    (union body
                           legs
                           rocker-assembly
                           neck
                           head-assembly
                           tail))
                   (delta #[(- (/ body-length 2)) (/ rocker-offset-y 2) 0]))
              (move model delta))))
    rocking-horse))

(make-viewer)
(set-shape (make-horse-shape) '((-2 . 2) (-2 . 2) (-2 . 2)) 100)
