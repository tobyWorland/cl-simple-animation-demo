
(in-package :animation)

;;; Take the printed form #<point@X=...,Y=...,ΔX=...,ΔY=...>
(defstruct (point
	    (:print-function
	     (lambda (point stream depth)
	       (declare (ignore depth))
	       (format stream "#<point@X=~A,Y=~A,ΔX=~A,ΔY=~A>"
		       (point-x point)
		       (point-y point)
		       (point-velocity-x point)
		       (point-velocity-y point)))))
  x
  y
  velocity-x
  velocity-y)

(defun generate-points (number-of-points max-x max-y min-vel-x max-vel-x min-vel-y max-vel-y)
  "Create a list of random points with the given parameters"
  (let (result)
    (dotimes (ignored number-of-points (nreverse result))
      (push (make-point :x (random-in-range 0 max-x)
			:y (random-in-range 0 max-y)
			:velocity-x (random-in-range min-vel-x max-vel-x)
			:velocity-y (random-in-range min-vel-y max-vel-y))
	    result))))

(defun distance-between-points (point-1 point-2)
  "Return the distance between the two points."
  (if (and point-1 point-2)
      (let ((diff-x (- (point-x point-2) (point-x point-1)))
	    (diff-y (- (point-y point-2) (point-y point-1))))
	(sqrt (+ (expt diff-x 2) (expt diff-y 2))))
      0))

(defun apply-velocity (point max-x max-y)
  "Return a new point with the position of the old POINT plus its VELOCITY, wrapping the position
back to 0 if it exceeds MAX-X or MAX-Y."
  (make-point :x (mod (+ (point-x point) (point-velocity-x point)) max-x)
	      :y (mod (+ (point-y point) (point-velocity-y point)) max-y)
	      :velocity-x (point-velocity-x point)
	      :velocity-y (point-velocity-y point)))

(defun distance-mapping-threshold (points threshold)
  "Return a list of POINTS and distances between those POINTS, only when the distance is less than
or equal to THRESHOLD.

Such as ((p1 (p2 . distance-from-p2-to-p1)
             (p3 . distance-from-p3-to-p1)
             ...)
         (p2 (p3 . distance-from-p3-to-p2)
             ...)
         ...)"
  ;; Avoid duplicates by only comparing the points that come after the current one
  (loop for (current-point . next-points) on points
	collect (cons current-point (loop for other-point in next-points
					  for distance = (distance-between-points current-point other-point)
					  when (<= distance threshold)
					    collect (cons other-point distance)))))
