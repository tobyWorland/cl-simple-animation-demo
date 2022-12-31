
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
  "Perform the update logic:
 - Update point positions
 - Draw points
 - Draw lines between points"
  ;; Clear screen
  (set-render-draw-color renderer 0 0 0 255)
  (render-clear renderer)

  ;; Calculate new point positions and render them
  (set-render-draw-color renderer 255 255 255 255)
  (setf *points* (mapcar (lambda (point)
			   (let ((new-point (apply-velocity point +width+ +height+)))
			     (render-point renderer new-point)
			     new-point))
			 *points*))

  ;; Render the lines between the points
  (render-lines-mapping renderer (distance-mapping-threshold *points* *max-line-distance*)
			*max-line-distance*)

  (render-present renderer))

(defun render-point (renderer point)
  "Draw a point as a 5x5 square"
  (let ((rect (make-rect (point-x point) (point-y point) 5 5)))
    (render-fill-rect renderer rect)
    (free-rect rect)))

(defun render-point-line (renderer point-1 point-2 alpha)
  "Draw a line between two points taking an ALPHA value to control transparency"
  (set-render-draw-color renderer 255 255 255 alpha)
  (let ((x1 (+ 2 (point-x point-1)))
	(y1 (+ 2 (point-y point-1)))
	(x2 (+ 2 (point-x point-2)))
	(y2 (+ 2 (point-y point-2))))
    (render-draw-line renderer x1 y1 x2 y2)))

(defun render-lines-mapping (renderer mapping threshold)
  "Draw lines based on the entries in a line mapping. See the DISTANCE-MAPPING-THRESHOLD function
for more information."
  (dolist (map mapping)
    (dolist (x (cdr map))
      (render-point-line renderer (car map) (car x) (round (* 255 (/ (- threshold (cdr x)) threshold)))))))

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
