
(in-package :animation)

(defconstant +width+ 900
  "Window width")

(defconstant +height+ 768
  "Window height")

(defparameter *points* (generate-points 100 +width+ +height+ -3 3 -3 3)
  "List of screen points to connect")

(defparameter *target-fps* 60
  "Frames per second to aim for")

(defparameter *max-line-distance* 200
  "The maximum distance a line can be drawn at")

(defun update (renderer)
  (set-render-draw-color renderer 0 0 0 255)
  (render-clear renderer)

  (render-lines-mapping renderer (distance-mapping-threshold *points* *max-line-distance*))

  (set-render-draw-color renderer 255 255 255 255)

  (setf *points* (mapcar (lambda (point)
			   (prog1 (apply-velocity point +width+ +height+)
			     (render-point renderer point)))
			 *points*))

  (render-present renderer))

(defun render-point (renderer point)
  (let ((rect (make-rect (point-x point) (point-y point) 5 5)))
    (render-fill-rect renderer rect)
    (free-rect rect)))

(defun render-point-line (renderer point-1 point-2 alpha)
  (set-render-draw-color renderer 255 255 255 alpha)
  (let ((x1 (+ 3 (point-x point-1)))
	(y1 (+ 3 (point-y point-1)))
	(x2 (+ 3 (point-x point-2)))
	(y2 (+ 3 (point-y point-2))))
    (sdl2:render-draw-line renderer x1 y1 x2 y2)))

(defun render-lines-mapping (renderer mapping)
  (dolist (map mapping)
    (dolist (x (cdr map))
      (render-point-line renderer (car map) (car x) (round (* 255 (/ (- *max-line-distance* (cdr x)) *max-line-distance*)))))))

(defun %run ()
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title "Animation (error)"
                              :w +width+
                              :h +height+
                              :flags '(:shown))
      (sdl2:with-renderer (renderer window :index -1 :flags '(:accelerated))
	(sdl2:set-render-draw-blend-mode renderer sdl2-ffi:+sdl-blendmode-blend+)
	(sdl2:with-event-loop nil
	  (:quit () t)
	  (:idle ()
		 (let ((start (get-ticks)))
		   (update renderer)
		   (let* ((elapsed (- (get-ticks) start))
			  (delay-for (round (- (/ 1000 *target-fps*) elapsed))))
		     (when (and delay-for (plusp delay-for))
		       (delay delay-for)))
		   (set-window-title
		    window
		    (format nil "Animation FPS ~A"
			    (round (/ 1000 (- (get-ticks) start))))))))))))

;;; Call me to start the animation
(defun run ()
  (sb-thread:make-thread #'%run))
