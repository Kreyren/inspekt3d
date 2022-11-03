;;; camera.scm
;;;
;;; Copyright 2018 Kavalogic, Inc.
;;;
;;; This file is part of Inspekt3d.
;;;
;;; Inspekt3d is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Inspekt3d is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (inspekt3d camera)
  #:use-module (inspekt3d gl-missing)
  #:use-module (oop goops)
  #:use-module (gl))

;;; class <rotation>
(define-class <rotation> ()
  (rotating? #:getter is-rotating? #:init-value #f)
  (screen-rot-x #:getter get-screen-rot-x #:init-value 0)
  (screen-rot-y #:getter get-screen-rot-y #:init-value 0)
  (screen-base-x)
  (screen-base-y)
  (last-screen-offset-x #:init-value 0)
  (last-screen-offset-y #:init-value 0))
(export <rotation>)
(export is-rotating? get-screen-rot-x get-screen-rot-y)

(define-method (rotation-start (self <rotation>)
                               (x <integer>)
                               (y <integer>))
  (slot-set! self 'screen-base-x x)
  (slot-set! self 'screen-base-y y)
  (slot-set! self 'rotating? #t))
(export rotation-start)

(define *rotation-scale* 360/500)

(define-method (rotation-rotate (self <rotation>)
                                (x <integer>)
                                (y <integer>))
  (let ((x' (+ (- x (slot-ref self 'screen-base-x))
               (slot-ref self 'last-screen-offset-x)))
        (y' (+ (- y (slot-ref self 'screen-base-y))
               (slot-ref self 'last-screen-offset-y))))
    (slot-set! self 'screen-rot-y (* *rotation-scale* x'))
    (slot-set! self 'screen-rot-x (* *rotation-scale* y'))))
(export rotation-rotate)

(define-method (rotation-end (self <rotation>)
                                (x <integer>)
                                (y <integer>))
  (slot-set! self 'rotating? #f)
  (slot-set! self 'last-screen-offset-x
             (+ (slot-ref self 'last-screen-offset-x)
                (- x (slot-ref self 'screen-base-x))))
  (slot-set! self 'last-screen-offset-y
             (+ (slot-ref self 'last-screen-offset-y)
                (- y (slot-ref self 'screen-base-y)))))
(export rotation-end)

;; class <zoom>
(define-class <zoom> ()
  ;; current zoom
  (zoom-value #:getter get-zoom #:init-value 1 #:init-keyword #:zoom-value)
  ;; how fast we zoom
  (factor #:init-keyword #:factor))
(export <zoom>)
(export get-zoom)
(define-method (zoom-in (self <zoom>))
  (let* ((factor (slot-ref self 'factor))
         (zoom-value (+ (slot-ref self 'zoom-value) factor)))
    (slot-set! self 'zoom-value zoom-value)))
(export zoom-in)
(define-method (zoom-out (self <zoom>))
  (let* ((factor (slot-ref self 'factor))
         (zoom-value (- (slot-ref self 'zoom-value) factor)))
    (slot-set! self 'zoom-value zoom-value)))
(export zoom-out)

;;; class <camera>
(define-class <camera> ()
  (width #:getter get-width #:setter set-width #:init-value 0)
  (height #:getter get-height #:setter set-height #:init-value 0)
  (eye #:getter get-eye #:setter set-eye #:init-value #(0 0 1))
  (center #:getter get-center #:setter set-center #:init-value #(0 0 0))
  (up #:getter get-up #:setter set-up #:init-value #(0 1 0))
  (rotator #:getter get-rotator #:init-form (make <rotation>))
  (zoomer #:getter get-zoomer
          #:init-form (make <zoom> #:factor 0.05 #:zoom-value 1)))
(export <camera>)
(export get-width set-width get-height set-height get-eye set-eye)
(export get-center set-center get-up set-up)
(export get-rotator get-zoomer)

(define (vec3-length v)
  (define (square e) (* e e))
  (sqrt (+ (square (vector-ref v 0))
           (square (vector-ref v 1))
           (square (vector-ref v 2)))))
(define (vec3-sub v1 v2)
  (vector (- (vector-ref v1 0)
             (vector-ref v2 0))
          (- (vector-ref v1 1)
             (vector-ref v2 1))
          (- (vector-ref v1 2)
             (vector-ref v2 2))))

(define-method (establish-modelview-matrix (self <camera>))
  (let ((eye (slot-ref self 'eye))
        (center (slot-ref self 'center))
        (up (slot-ref self 'up)))
    (set-gl-matrix-mode (matrix-mode modelview))
    (gl-load-identity)
    (let ((distance (vec3-length (vec3-sub eye center)))
          (rotator (slot-ref self 'rotator)))
      (gl-translate 0 0 (- distance))
      (gl-rotate (slot-ref rotator 'screen-rot-x) 1 0 0)
      (gl-rotate (slot-ref rotator 'screen-rot-y) 0 1 0)
      (gl-translate 0 0 distance)
      (glu-look-at (vector-ref eye 0)
                   (vector-ref eye 1)
                   (vector-ref eye 2)
                   (vector-ref center 0)
                   (vector-ref center 1)
                   (vector-ref center 2)
                   (vector-ref up 0)
                   (vector-ref up 1)
                   (vector-ref up 2)))))
(export establish-modelview-matrix)

(define (bounds-length l)
  (let* ((x (car l))
         (x-min (car x))
         (x-max (cdr x))
         (y (cadr l))
         (y-min (car y))
         (y-max (cdr y))
         (z (caddr l))
         (z-min (car z))
         (z-max (cdr z)))
    (vec3-length (vector (- x-max x-min)
                         (- y-max y-min)
                         (- z-max z-min)))))

(define-method (establish-projection-matrix (self <camera>)
                                            (bounds <list>))
  (let* ((r (bounds-length bounds))
         (dist-2-eye (vec3-length (vec3-sub (slot-ref self 'eye)
                                            (slot-ref self 'center))))
         (viewport-aspect (/ (slot-ref self 'height)
                             (slot-ref self 'width)))
         (zoom (slot-ref (slot-ref self 'zoomer) 'zoom-value))
         (min (vector (- r) (- r) (- r)))
         (min-x (vector-ref min 0))
         (min-y (vector-ref min 1))
         (max (vector r r r))
         (max-x (vector-ref max 0))
         (max-y (vector-ref max 1))
         (window-aspect (/ (- max-y min-y) (- max-x min-x))))
    ;; calculate viewport parameters
    (if (> viewport-aspect window-aspect)
        ;; viewport taller than it needs to be
        (let ((new-height (* viewport-aspect (- max-x min-x)))
              (y-mid (/ (+ min-y max-y) 2)))
          (vector-set! max 1 (+ y-mid (* 1/2 new-height)))
          (vector-set! min 1 (- y-mid (* 1/2 new-height))))
        ;; viewport wider than it needs to be
        (let ((new-width (/ (- max-y min-y) viewport-aspect))
              (x-mid (/ (+ min-x max-x) 2)))
          (vector-set! max 0 (+ x-mid (* 1/2 new-width)))
          (vector-set! min 0 (- x-mid (* 1/2 new-width)))))
    ;; do the projection
    (let ((new-min (vector (/ (vector-ref min 0) zoom)
                           (/ (vector-ref min 1) zoom)
                           (vector-ref min 2)))
          (new-max (vector (/ (vector-ref max 0) zoom)
                           (/ (vector-ref max 1) zoom)
                           (vector-ref max 2))))
      ;; use an orthographic projection using adjusted model boundaries
      (gl-ortho (vector-ref new-min 0) (vector-ref new-max 0)
                (vector-ref new-min 1) (vector-ref new-max 1)
                (vector-ref new-min 2) (vector-ref new-max 2)))))
(export establish-projection-matrix)
