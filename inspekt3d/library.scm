;;; library.scm
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

(eval-when (expand load eval)
  (load-extension "libfive-guile" "scm_init_libfive_modules"))

(define-module (inspekt3d library)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (libfive kernel))

(define libfive-lib (dynamic-link "libfive"))
(define libfive-guile-lib (dynamic-link "libfive-guile"))

;;; low level FFI definitions
(define libfive-region3-struct-type (list float float float float float float))
(define (make-region3-struct r)
  (let* ((x-bounds (car r))
         (y-bounds (cadr r))
         (z-bounds (caddr r))
         (x-min (car x-bounds)) (x-max (cdr x-bounds))
         (y-min (car y-bounds)) (y-max (cdr y-bounds))
         (z-min (car z-bounds)) (z-max (cdr z-bounds)))
    (make-c-struct libfive-region3-struct-type
                   (list x-min x-max y-min y-max z-min z-max))))
(define libfive-mesh-struct (list '* '* uint32 uint32))
(define libfive-vec3-struct (list float float float))
(define libfive-tri-struct (list uint32 uint32 uint32))
(define %libfive-tree-render-mesh
  (pointer->procedure '*
                      (dynamic-func "libfive_tree_render_mesh" libfive-lib)
                      (list '* libfive-region3-struct-type float)))
(define %libfive-delete-mesh
  (pointer->procedure void
                      (dynamic-func "libfive_mesh_delete" libfive-lib)
                      (list '*)))

;;; foreign mesh class
(define-class <mesh> ()
  (mesh-ptr #:getter get-mesh-ptr #:init-keyword #:mesh-ptr #:init-value #nil)
  (region #:getter get-region #:init-keyword #:region))
(export <mesh>)
(define (make-mesh m r) (make <mesh> #:mesh-ptr m #:region r))
(define-method (get-mesh-members (self <mesh>))
  (let ((mesh-struct-list (parse-c-struct (get-mesh-ptr self) libfive-mesh-struct)))
    (values (car mesh-struct-list) ; verts
            (cadr mesh-struct-list) ; tris
            (caddr mesh-struct-list) ; tri_count
            (cadddr mesh-struct-list)))) ; vert_count
(export get-mesh-members)
;;;   delete low level structure
(define-method (delete-mesh (self <mesh>))
  (let ((mesh-ptr (get-mesh-ptr self)))
    (unless (null? mesh-ptr)
      (%libfive-delete-mesh mesh-ptr)
      (slot-set! self 'mesh-ptr #nil))))
(export delete-mesh)
;;;
(define-method (shape->in-memory-mesh (s <shape>)
                                      (r <list>)
                                      (res <number>))
  "shape->mesh-in-memory-mesh shape region resolution
  Convert a libfive shape into an in-memory <mesh> object suitable for
  displaying."
  (make-mesh (%libfive-tree-render-mesh
              ;; hack, depends on layout of shape struct!:
              (dereference-pointer (unwrap-shape s))
              (make-region3-struct r)
              res)
              r))
(export shape->in-memory-mesh)
