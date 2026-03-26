(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-simd))

(defpackage :mandelbrot
  (:use :cl)
  (:export z->z^2+c z->z^2+c-impl)
  (:local-nicknames
   (:avx2 :sb-simd-avx2)
   (:avx :sb-simd-avx)
   (:sse4.1 :sb-simd-sse4.1)
   (:sse4.2 :sb-simd-sse4.2)))

(in-package :mandelbrot)

(eval-when (:compile-toplevel :execute)
  (defun symjoin (package &rest xs)
    (intern (reduce (lambda (x y) (format nil "~A~A" x y)) xs) package)))

(eval-when (:compile-toplevel :execute)
(defmacro define-|z->z^2+c|-impl (package len)
  (let ((vec-aref (symjoin package :f32. len :-aref))
        (make-vec (symjoin package :f32. len))
        (vec+ (symjoin package :f32. len :+))
        (vec- (symjoin package :f32. len :-))
        (vec< (symjoin package :f32. len :<))
        (vec* (symjoin package :f32. len :*))
        (u32-values-vec (symjoin package :u32. len :-values))
        (vec-if (symjoin package :f32. len :-if)))
    `(progn
       (defun z->z^2+c-impl (cx cy max-iter)
         (declare (type (simple-array sb-simd:f32 *) cx cy)
                  (type (unsigned-byte 32) max-iter)
                  (optimize (speed 3) (safety 0)))
         (when (/= (length cx) (length cy))
           (error "Length of Cx and Cy don't match."))
         (let ((result (make-array (length cx) :element-type 'sb-simd:f32))
               (n (length cx)))
           (macrolet ((iter (gap endp scalarp + - * < aref imm ->values if)
                        `(do ((index 0 (+ index ,gap)))
                             (,endp)
                           (declare (type (unsigned-byte 32) index n))
                           (let ((cx (,aref cx index))
                                 (cy (,aref cy index))
                                 (zx (,imm 0))
                                 (zy (,imm 0))
                                 (count (,imm 0)))
                             ,@(if scalarp
                                   '((declare (type sb-simd:f32 cx cy zx zy)))
                                   nil)
                             (block iterate
                               (dotimes (_ max-iter)
                                 (let* ((zx^2 (,* zx zx))
                                        (zy^2 (,* zy zy))
                                        (mag^2 (,+ zx^2 zy^2))
                                        (mask (,< mag^2 (,imm 4))))
                                   ,@(if scalarp
                                         '((declare (type sb-simd:f32 zx^2 zy^2 mag^2)))
                                         nil)
                                   (when (multiple-value-call #'= 0 (,->values mask))
                                     (return-from iterate))
                                   (let ((zx* (,+ (,- zx^2 zy^2) cx))
                                         (zy* (,+ (,* zx zy (,imm 2)) cy)))
                                     ,@(if scalarp
                                           '((declare (type sb-simd:f32 zx* zy*)))
                                           nil)
                                     (setf zx (,if mask zx* zx)
                                           zy (,if mask zy* zy))
                                     (setf count (,+ count (,if mask (,imm 1) (,imm 0))))))))
                             (setf (,aref result index) count)))))
             (iter 8 (>= index (- n 8)) nil ,vec+ ,vec- ,vec* ,vec< ,vec-aref ,make-vec ,u32-values-vec ,vec-if)
             (iter 1 (>= index n) t + - * < aref float (lambda (x) (if x 1 0)) if))
           result))))))

(declaim (ftype (function ((simple-array sb-simd:f32 *)
                           (simple-array sb-simd:f32 *)
                           (unsigned-byte 32))
                          (simple-array sb-simd:f32 *))
                z->z^2+c-impl))

(sb-simd-internals:instruction-set-case
  (:avx2 (define-|z->z^2+c|-impl :avx2 8))
  (:avx (define-|z->z^2+c|-impl :avx 8))
  (:sse4.2 (define-|z->z^2+c|-impl :sse4.2 4))
  (:sse4.1 (define-|z->z^2+c|-impl :sse4.1 4)))

(declaim (inline z->z^2+c))
(defun z->z^2+c (cx cy max-iter)
"Compute Mandelbrot set iteration counts

For each complex point C = cx + i*cy, iterates z_{n+1} = z_n^2 + C
starting from z_0 = 0 until |z| >= 2 (escape) or MAX-ITER is reached.

Args:
  CX       - Real parts, (SIMPLE-ARRAY SB-SIMD:F32 *)
  CY       - Imaginary parts, (SIMPLE-ARRAY SB-SIMD:F32 *)
  MAX-ITER - Maximum iterations, (UNSIGNED-BYTE 32)

Returns:
  Result array of SB-SIMD:F32 containing iteration counts per point.
  Non-escaping points (reaching MAX-ITER) return MAX-ITER.

Example:

  (let ((arr1 (make-array 18 :element-type 'sb-simd:f32))
        (arr2 (make-array 18 :element-type 'sb-simd:f32)))
    (dotimes (i 18)
      (setf (aref arr1 i) (* 0.2 i))
      (setf (aref arr2 i) (* 0.1 i)))
    (mandelbrot:z->z^2+c arr1 arr2 1000))

Signals:
  ERROR if (LENGTH CX) /= (LENGTH CY)"
  (declare (type (simple-array sb-simd:f32 *) cx cy)
           (type (unsigned-byte 32) max-iter))
  (z->z^2+c-impl cx cy max-iter))

(define-compiler-macro z->z^2+c (cx cy max-iter)
  `(z->z^2+c-impl (the (simple-array sb-simd:f32 *) ,cx)
                  (the (simple-array sb-simd:f32 *) ,cy)
                  (the (unsigned-byte 32) ,max-iter)))

(when (and (boundp '*mandel-brot-debug*) (eval '(eq *mandel-brot-debug* t)))
  (disassemble 'mandelbrot:z->z^2+c-impl)

  (let ((arr1 (make-array 18 :element-type 'sb-simd:f32))
        (arr2 (make-array 18 :element-type 'sb-simd:f32)))
    (dotimes (i 18)
      (setf (aref arr1 i) (* 0.2 i))
      (setf (aref arr2 i) (* 0.1 i)))
    (terpri)
    (print arr1)
    (print arr2)
    (mandelbrot:z->z^2+c arr1 arr2 1000)))
