(defpackage :mandelbrot-draw
  (:use :cl :mandelbrot)
  (:export *width* *height* *maxiter* show dump-png))

(in-package :mandelbrot-draw)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "sdl2")
  (ql:quickload "imago"))

(eval-when (:execute :compile-toplevel)
(defmacro with-window-renderer
    ((window renderer)
     (&key
      (title "SDL2 Window")
      (screen-width 800)
      (screen-height 600)
      (window-flags ''(:shown)))
     &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title ,title
                        :w ,screen-width
                        :h ,screen-height
                        :flags ,window-flags)
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body)))))

(defparameter *height* 400 "default height")
(defparameter *width* 400 "default width")
(defparameter *maxiter* 1000 "default max iteration times")

(defun iter->rgb (iter maxiter)
  (declare (type (unsigned-byte 32) iter maxiter))
  (declare (optimize (speed 3)))
  (when (>= iter maxiter)
    (return-from iter->rgb (list 0 0 0)))
  (let ((freq 0.1) (phase-r 0) (phase-g 2) (phase-b 4))
    (let ((time (* iter freq)))
      (labels ((calc (phase) (round (* 255 0.5 (1+ (sin (+ time phase)))))))
        (declare (inline calc))
        (list (calc phase-r) (calc phase-g) (calc phase-b))))))

(defun draw* (width height maxiter)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (unsigned-byte 32) width height maxiter))
  (let* ((map-x (make-array (* width height) :element-type 'single-float))
         (map-y (make-array (* width height) :element-type 'single-float))
         (width-scaling (* 2.0 (/ (float width 1.0) (max width height))))
         (height-scaling (* 2.0 (/ (float height 1.0) (max width height))))
         (half-width (/ width 2.0))
         (half-height (/ height 2.0)))
    (dotimes (x width)
      (dotimes (y height)
        (let ((pos (+ x (* y width))))
          (setf (aref map-x pos) (* width-scaling (/ (- x half-width) half-width)))
          (setf (aref map-y pos) (* height-scaling (/ (- y half-height) half-height))))))
    (map 'simple-vector
         (lambda (iter) (iter->rgb (round iter) maxiter))
         (z->z^2+c map-x map-y maxiter))))

(declaim (inline draw))
(defun draw (&optional (width *width*) (height *height*) (maxiter *maxiter*))
  (declare (type (unsigned-byte 32) width height maxiter))
  (draw* width height maxiter))

(defun show (&optional (width *width*) (height *height*) (maxiter *maxiter*))
  "Open SDL2 window displaying Mandelbrot set."
  (sdl2:make-this-thread-main
   (lambda ()
     (let ((map (draw width height maxiter)))
       (with-window-renderer (window renderer)
           (:title "mandelbrot" :screen-width width :screen-height height)
         (dotimes (x width)
           (dotimes (y height)
             (sdl2:with-rects ((fill-rect x y 1 1))
               (multiple-value-call #'sdl2:set-render-draw-color
                 renderer
                 (let ((pos (+ x (* y width))))
                   (values-list (aref map pos)))
                 0)
               (sdl2:render-fill-rect renderer fill-rect))))
         (sdl2:render-present renderer)
         (block exit
           (loop
            (sdl2:with-event-loop (:method :poll)
              (:quit () (return-from exit t))))))))))

(defun dump-png (name &optional (width *width*) (height *height*) (maxiter *maxiter*))
  "Write Mandelbrot set to PNG file NAME."
  (imago:write-png
   (imago:make-rgb-image-from-pixels
    (let ((map (draw width height maxiter)))
      (let ((image (make-array (list height width)
                    :element-type 'imago:rgb-pixel
                    :initial-element (imago:make-color 0 255 255))))
        (dotimes (x width)
          (dotimes (y height)
            (let ((pos (+ x (* y width))))
              (setf (aref image y x)
                    (apply #'imago:make-color (aref map pos))))))
        image)))
   name))
