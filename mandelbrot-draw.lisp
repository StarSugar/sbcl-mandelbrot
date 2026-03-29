(defpackage :mandelbrot-draw
  (:use :cl :mandelbrot)
  (:export draw))

(in-package :mandelbrot-draw)

(ql:quickload "sdl2")

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
         ,@body))))

(defparameter *height* 400)
(defparameter *width* 400)
(defparameter *maxiter* 1000)

(defun iter->rgb (iter maxiter)
  (declare (type (unsigned-byte 32) iter maxiter))
  (declare (optimize (speed 3)))
  (when (>= iter maxiter)
    (return-from iter->rgb (values 0 0 0)))
  (let ((freq 0.1) (phase-r 0) (phase-g 2) (phase-b 4))
    (let ((time (* iter freq)))
      (labels ((calc (phase) (round (* 255 0.5 (1+ (sin (+ time phase)))))))
        (declare (inline calc))
        (values (calc phase-r) (calc phase-g) (calc phase-b))))))

(defun draw* (width height maxiter)
  (declare (optimize (speed 3)))
  (let ((map-x (make-array (* width height) :element-type 'single-float))
        (map-y (make-array (* width height) :element-type 'single-float))
        (half-width (/ width 2.0))
        (half-height (/ height 2.0)))
    (dotimes (x width)
      (dotimes (y height)
        (let ((pos (+ x (* y width))))
          (setf (aref map-x pos) (* 2.0 (/ (- x half-width) half-width)))
          (setf (aref map-y pos) (* 2.0 (/ (- y half-height) half-height))))))
    (let ((res (time (z->z^2+c map-x map-y maxiter))))
      (with-window-renderer (window renderer)
          (:title "mandelbrot" :screen-width width :screen-height height)
        (dotimes (x width)
          (dotimes (y height)
            (sdl2:with-rects ((fill-rect x y 1 1))
              (multiple-value-call #'sdl2:set-render-draw-color
                renderer
                (let ((pos (+ x (* y width))))
                  (iter->rgb (round (aref res pos)) maxiter))
                0)
              (sdl2:render-fill-rect renderer fill-rect))))
        (sdl2:render-present renderer)
        (block exit
          (loop
           (sdl2:with-event-loop (:method :poll)
             (:quit () (return-from exit t)))))))))

(defun draw (&optional (width *width*) (height *height*) (maxiter *maxiter*))
  (sdl2:make-this-thread-main
   (lambda () (draw* width height maxiter))))
