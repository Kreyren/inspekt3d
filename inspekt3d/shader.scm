;;; shader.scm
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

(define-module (inspekt3d shader)
  #:use-module (gl)
  #:use-module (inspekt3d gl-missing)
  #:use-module (system foreign)
  #:use-module (ice-9 format)
  #:use-module (oop goops))

;;;   shader
(define-class <shader> ()
  (vertex-shader #:init-value #nil)
  (fragment-shader #:init-value #nil)
  (shader-program  #:init-value #nil))
(export <shader>)

;;; shader source
;;;   vertex
(define *vertex-shader-source*
"#version 120
varying vec3 ec_pos;

void main() {
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;

    ec_pos = gl_Position.xyz;
}
")

;;;   fragment
(define *fragment-shader-source*
"#version 120

varying vec3 ec_pos;

void main() {
    vec3 base3 = vec3(0.99, 0.96, 0.89);
    vec3 base2 = vec3(0.92, 0.91, 0.83);
    vec3 base00 = vec3(0.40, 0.48, 0.51);

    vec3 ec_normal = normalize(cross(dFdx(ec_pos), dFdy(ec_pos)));

    float a = dot(ec_normal, vec3(0.0, 0.0, 1.0));
    float b = dot(ec_normal, vec3(-0.57, -0.57, 0.57));

    gl_FragColor = vec4((a*base2 + (1-a)*base00)*0.5 +
                        (b*base3 + (1-b)*base00)*0.5, 1.0);
}
")

(define (print-shader-log shader shader-type)
    (format #t "~%*** ~a shader compile failed!~%" shader-type)
    (format #t "*** Log:~%")
    (format #t "~a~%" (gl-get-shader-info-log shader))
    (format #t "***~%"))

(define (print-program-log program)
    (format #t "~%*** Program link failed!~%")
    (format #t "*** Log:~%")
    (format #t "~a~%" (gl-get-program-info-log program))
    (format #t "***~%"))

(define string-array-type (list '*))

(define-method (setup (self <shader>))
  (let ((vert-shader (gl-create-shader (version-2-0 vertex-shader)))
        (frag-shader (gl-create-shader (version-2-0 fragment-shader))))
    ;; compile the vertex and fragment shaders
    ;;   vertex shader
    (let* ((vert-shader-src-str (string->pointer *vertex-shader-source*))
           (vert-shader-src-array (make-c-struct
                                   string-array-type
                                   (list vert-shader-src-str))))
      (gl-shader-source vert-shader 1 vert-shader-src-array %null-pointer)
      (gl-compile-shader vert-shader)
      (if (eq? (gl-get-shader vert-shader (version-2-0 compile-status))
               (boolean true))
          (slot-set! self 'vertex-shader vert-shader)
          (print-shader-log vert-shader "Vertex")))
    ;;   fragment shader
    (let* ((frag-shader-src-str (string->pointer *fragment-shader-source*))
           (frag-shader-src-array (make-c-struct
                                   string-array-type
                                   (list frag-shader-src-str))))
      (gl-shader-source frag-shader 1 frag-shader-src-array %null-pointer)
      (gl-compile-shader frag-shader)
      (if (eq? (gl-get-shader frag-shader (version-2-0 compile-status))
               (boolean true))
          (slot-set! self 'fragment-shader frag-shader)
          (print-shader-log frag-shader "Fragment"))))
  ;; attach the shaders to the program and link
  (let ((vertex-shader (slot-ref self 'vertex-shader))
        (fragment-shader (slot-ref self 'fragment-shader)))
    (unless (or (null? vertex-shader) (null? fragment-shader))
      ;; attach shaders and link shader program
      (let ((program (gl-create-program)))
        (gl-attach-shader program vertex-shader)
        (gl-attach-shader program fragment-shader)
        (gl-link-program program)
        (if (eq? (gl-get-program program (version-2-0 link-status))
                 (boolean true))
            (begin
              (gl-use-program program)
              (slot-set! self 'shader-program program))
            (print-program-log program))))))
(export setup)

(define-method (cleanup (shader <shader>))
  (let ((vertex-shader (slot-ref shader 'vertex-shader)))
    (unless (null? vertex-shader) (gl-delete-shader vertex-shader)))
  (let ((fragment-shader (slot-ref shader 'fragment-shader)))
    (unless (null? fragment-shader) (gl-delete-shader fragment-shader)))
  (let ((shader-program (slot-ref shader 'shader-program)))
    (unless (null? shader-program) (gl-delete-program shader-program))))
(export cleanup)
