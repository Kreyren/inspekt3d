;;; viewer.scm
;;;
;;; Copyright 2018 Kavalogic, Inc.
;;; Copyright (C) 2014 Free Software Foundation, Inc.
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

;;; Derived from Guile-OpenGL 0.1.0
;;; File: ./examples/particle-system/vbo.scm
;;; Original License: LGPL 3+

(define-module (inspekt3d viewer)
  #:use-module (inspekt3d library)
  #:use-module (inspekt3d camera)
  #:use-module (inspekt3d shader)
  #:use-module (inspekt3d gl-missing)
  #:use-module (glut)
  #:use-module (gl)
  #:use-module (glu)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:use-module (system foreign)
  #:use-module (oop goops))

;;; Globals
(define *full-screen?* #f)
;;;   object buffers
(define *vertex-buffer* 0)
(define *index-buffer* 0)
;;;   main window
(define *main-window* #nil)
;;;   shape
(define *shape-changed?* #f)
;;;     set by external thread, used when shape is changed
(define *new-mesh* #nil)
(define *new-bounds* #nil)
;;;     set by viewer thread, used to display
(define *mesh* #nil)
(define *bounds* #nil)
;;;   mutexes
(define *run-mutex* (make-mutex))
(define *shape-mutex* (make-mutex))
;;;   camera
(define *camera* #nil)
;;;   shader
(define *shader* #nil)

(define (draw-shape)
  (unless (null? *mesh*)
    (gl-enable-client-state (enable-cap vertex-array))
    (gl-bind-buffer (version-1-5 array-buffer) *vertex-buffer*)
    (set-gl-vertex-array (vertex-pointer-type float) #f)
    (gl-bind-buffer (version-1-5 element-array-buffer) *index-buffer*)
    (gl-draw-elements (begin-mode triangles)
                      (* 3 (call-with-values
                               (lambda () (get-mesh-members *mesh*))
                             (lambda (v t triangle-count vc) triangle-count)))
                      (data-type unsigned-int)
                      #f)
    (gl-disable-client-state (enable-cap vertex-array))
    (gl-disable-client-state (enable-cap index-array))))

(define (exit-viewer)
  (unless (null? *main-window*) (glut-destroy-window *main-window*)))

(define (cleanup-for-exit)
  ;; clean up the shaders
  (cleanup *shader*)
  ;; cleanup up the mesh
  (unless (null? *mesh*) (delete-mesh *mesh*))
  (set! *main-window* #nil)
  (set-shape #nil #nil #nil))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))

  (establish-modelview-matrix *camera*)

  (draw-shape)

  ;; With double-buffering, swap-buffers will wait for the frame to be shown,
  ;; which limits this program to the frame rate.
  (swap-buffers))

(define (reshape)
  ;; use the whole window
  (let ((width (get-width *camera*))
        (height (get-height *camera*)))
    (gl-viewport 0 0 width height))

  ;; update the projection matrix
  (set-gl-matrix-mode (matrix-mode projection))
  (gl-load-identity)
  (unless (null? *bounds*)
    (establish-projection-matrix *camera* *bounds*))

  (set-gl-matrix-mode (matrix-mode modelview)))

(define (on-reshape width height)
  ;; update the camera
  (set-width *camera* width)
  (set-height *camera* height)

  (reshape))

(define (on-special keycode x y)
  #f)

(define (on-mouse button state x y)
  (cond (;; left button rotates
         (eq? button (button-name left-button))
         (let ((rotator (get-rotator *camera*)))
           (if (eq? state (button-state down))
               (rotation-start rotator x y)
               (rotation-end rotator x y))))
        (;; mouse wheel button rotate upwards
         (eq? button 3)
         (let ((zoomer (get-zoomer *camera*)))
           (zoom-in zoomer)
           (reshape)
           (post-redisplay *main-window*)))
        (;; mouse wheel button rotate downwards
         (eq? button 4)
         (let ((zoomer (get-zoomer *camera*)))
           (zoom-out zoomer)
           (reshape)
         (post-redisplay *main-window*)))))

(define (on-motion x y)
  (let ((rotator (get-rotator *camera*)))
    (if (is-rotating? rotator)
        (begin
          ;; update rotation
          (rotation-rotate rotator x y)
          (post-redisplay *main-window*)))))

(define (on-visibility visible?)
  #f)

(define (setup-for-display)
  (let ((need-redisplay? #f))
    (when (try-mutex *shape-mutex*)
      (with-throw-handler #t
        (lambda ()
          (when *shape-changed?*
            (unless (null? *mesh*)
              (delete-mesh *mesh*))
            (set! *mesh* *new-mesh*)
            (set! *bounds* *new-bounds*)
            (set! *new-mesh* #nil)
            (set! *new-bounds* #nil)
            (set! *shape-changed?* #f)
            (set! need-redisplay? #t)))
        #f)
      (unlock-mutex *shape-mutex*))
    (when need-redisplay?
      (receive (vertices triangles triangle-count vertex-count)
          (get-mesh-members *mesh*)
        ;; set vertex buffer
        (gl-delete-buffer *vertex-buffer*) ; no-op if 0
        (set! *vertex-buffer* (gl-generate-buffer))
        (gl-bind-buffer (version-1-5 array-buffer) *vertex-buffer*)
        (set-gl-buffer-data (version-1-5 array-buffer)
                            vertices
                            (version-1-5 static-draw)
                            (* 3 (sizeof float) vertex-count))
        ;; set index buffer
        (gl-delete-buffer *index-buffer*) ; no-op if 0
        (set! *index-buffer* (gl-generate-buffer))
        (gl-bind-buffer (version-1-5 element-array-buffer) *index-buffer*)
        (set-gl-buffer-data (version-1-5 element-array-buffer)
                            triangles
                            (version-1-5 static-draw)
                            (* 3 (sizeof uint32) triangle-count))
        ;;; unbind buffers
        (gl-bind-buffer (version-1-5 array-buffer) 0)
        (gl-bind-buffer (version-1-5 element-array-buffer) 0)))
    need-redisplay?))

(define (on-idle)
  (when (setup-for-display)
    (reshape)
    (post-redisplay *main-window*)))

(define (on-keyboard keycode x y)
  (let ((c (integer->char keycode)))
    (case c
      ((#\f)
       (set! *full-screen?* (not *full-screen?*))
       (full-screen *main-window* *full-screen?*))
      ((#\esc #\etx #\q)
       (exit-viewer))
      ((#\])
       (let ((zoomer (get-zoomer *camera*)))
         (zoom-in zoomer)
         (reshape)
         (post-redisplay *main-window*)))
      ((#\[)
        (let ((zoomer (get-zoomer *camera*)))
          (zoom-out zoomer)
          (reshape)
          (post-redisplay *main-window*))))))

(define (register-glut-callbacks)
  ;; The trampolines allow the handlers to be overridden at runtime by
  ;; an attached Guile REPL client.
  (set-display-callback (lambda () (on-display)))
  (set-reshape-callback (lambda (w h) (on-reshape w h)))
  (set-keyboard-callback (lambda (k x y) (on-keyboard k x y)))
  (set-special-callback (lambda (k x y) (on-special k x y)))
  (set-mouse-callback (lambda (b s x y) (on-mouse b s x y)))
  (set-motion-callback (lambda (x y) (on-motion x y)))
  (set-visibility-callback (lambda (visible?) (on-visibility visible?)))
  (set-idle-callback (lambda () (on-idle))))

(define* (run-main #:key
                   (position #f)
                   (size #f)
                   (title "Inspekt3d"))
  (initialize-glut #:window-position position
                   #:window-size size
                   #:display-mode (display-mode rgba alpha double depth))
  (set! *main-window* (make-window title))
  (set! *camera* (make <camera>))
  (set! *shader* (make <shader>))
  (setup *shader*)

  (register-glut-callbacks)
  (glut-set-option (glut-option glut-action-on-window-close)
                   glut-close-action-mainloop-returns)
  (set-gl-clear-color 0 0 0 1)
  (set-gl-clear-depth 1)

  (gl-enable (enable-cap depth-test))
  (setup-for-display)

  (glut-main-loop)

  (cleanup-for-exit)
  (unlock-mutex *run-mutex*))

(define (viewer-main args)
  (if (not (try-mutex *run-mutex*))
      (error "Viewer already running.")
      (apply run-main args)))

(define (set-shape new-shape new-bounds new-resolution)
  "set-shape shape bounds resolution
   Sets the shape to display.  When the shape parameter is #nil, this
   function deallocates the current mesh."
  (lock-mutex *shape-mutex*)
  (with-throw-handler #t
    (lambda ()
      (unless (null? *new-mesh*)
        (delete-mesh *new-mesh*))
      (if (null? new-shape)
          (begin
            (set! *new-mesh* #nil)
            (set! *new-bounds* #nil))
          (begin
            (set! *new-mesh*
                  (shape->in-memory-mesh new-shape new-bounds new-resolution))
            (set! *new-bounds* new-bounds)))
      (set! *shape-changed?* #t))
      #f)
  (unlock-mutex *shape-mutex*))
(export set-shape)

;;; TODO: make multiple viewers possible (using a class)
;;; TODO: for now, only one viewer allowed
(define (make-viewer . args)
  (call-with-new-thread
   (lambda ()
     (viewer-main args))))
(export make-viewer)
