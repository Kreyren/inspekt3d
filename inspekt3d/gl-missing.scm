;;; gl-missing.scm
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

(define-module (inspekt3d gl-missing)
  #:use-module ((system foreign) #:renamer (symbol-prefix-proc 'ffi:))
  #:use-module (gl types)
  #:use-module (gl enums)
  #:use-module (gl runtime)
  #:use-module ((gl low-level) #:renamer (symbol-prefix-proc '%))
  #:use-module ((glu low-level) #:renamer (symbol-prefix-proc '%))
  #:use-module (glut)
  #:use-module (glut types)
  #:use-module (glut runtime)
  #:use-module ((glut low-level) #:renamer (symbol-prefix-proc '%)))


;;; missing GL functions
(re-export (%glDrawElements . gl-draw-elements))
;;;   lighting
(re-export (%glLightfv . gl-light))
(re-export (%glLightModelfv . gl-light-model))
(re-export (%glMaterialfv . gl-material))
;;;   shader
(re-export (%glCreateShader . gl-create-shader))
(re-export (%glShaderSource . gl-shader-source))
(re-export (%glCompileShader . gl-compile-shader))
(define (gl-get-shader shader pname)
  (let ((params (u32vector 0)))
    (%glGetShaderiv shader pname params)
    (u32vector-ref params 0)))
(export gl-get-shader)
;;;     shader program
(re-export (%glCreateProgram . gl-create-program))
(re-export (%glAttachShader . gl-attach-shader))
(re-export (%glLinkProgram . gl-link-program))
(define (gl-get-program program pname)
  (let ((params (u32vector 0)))
    (%glGetProgramiv program pname params)
    (u32vector-ref params 0)))
(export gl-get-program)
(re-export (%glUseProgram . gl-use-program))
(re-export (%glGetUniformLocation . gl-get-uniform-location))
(define (gl-uniform-float loc x . rest)
  (case (length rest)
    ((0) (%glUniform1f loc x))
    ((1) (apply %glUniform2f loc x rest))
    ((2) (apply %glUniform3f loc x rest))
    ((3) (apply %glUniform4f loc x rest))))
(export gl-uniform-float)
(define (gl-get-shader-info-log shader)
  (let* ((log-length-c (u32vector 0))
         (log-length (gl-get-shader shader (version-2-0 info-log-length))))
    (if (<= log-length 1)
        #f
        (let* ((log-string (make-string (1- log-length) #\space))
               (log-string-c (ffi:string->pointer log-string)))
          (%glGetShaderInfoLog shader log-length log-length-c log-string-c)
          (ffi:pointer->string log-string-c)))))
(export gl-get-shader-info-log)
(define (gl-get-program-info-log program)
  (let* ((log-length-c (u32vector 0))
         (log-length (gl-get-program program (version-2-0 info-log-length))))
    (if (<= log-length 1)
        #f
        (let* ((log-string (make-string (1- log-length) #\space))
               (log-string-c (ffi:string->pointer log-string)))
          (%glGetProgramInfoLog program log-length log-length-c log-string-c)
          (ffi:pointer->string log-string-c)))))
(export gl-get-program-info-log)
(re-export (%glDeleteShader . gl-delete-shader))
(re-export (%glDeleteProgram . gl-delete-program))

;;; missing GLU functions
(re-export (%gluLookAt . glu-look-at))

;;; missing GLUT functions (need Freeglut)
(define-enumeration glut-option
  (glut-action-on-window-close #x01F9))
(export glut-option)
(define glut-close-action-mainloop-returns 1)
(export glut-close-action-mainloop-returns)
(define-glut-procedure (glutSetOption (e-what GLenum)
                                      (value int)
                                      -> void) #f)
(export (glutSetOption . glut-set-option))
(define (glut-destroy-window window)
  (%glutDestroyWindow (window-id window)))
(export glut-destroy-window)
