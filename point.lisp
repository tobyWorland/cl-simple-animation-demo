
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
  (let ((diff-x (- (point-x point-2) (point-x point-1)))
	(diff-y (- (point-y point-2) (point-y point-1))))
    (sqrt (+ (expt diff-x 2) (expt diff-y 2)))))

(defun apply-velocity (point max-x max-y)
  "Return a new point with the position of the old POINT with its VELOCITY applied."
  (make-point :x (mod (+ (point-x point) (point-velocity-x point)) max-x)
	      :y (mod (+ (point-y point) (point-velocity-y point)) max-y)
	      :velocity-x (point-velocity-x point)
	      :velocity-y (point-velocity-y point)))

(defun distance-mapping (points)
  "Return a list in the form ((p1 (p2 . dist) (p3 . dist) ...)...)

Where p1 is a point and p2 p3 are other points in relation to p1."
  ;; Half the list as I don't want duplicates
  (let* ((boundary-index (floor (/ (length points) 2)))
	 (first-points (subseq points 0 boundary-index))
	 (other-points (subseq points boundary-index))
	 result)
    (dolist (first-point first-points (nreverse result))
      (push (cons first-point (mapcar (lambda (second-point)
					(cons second-point
					      (distance-between-points first-point second-point)))
				      other-points))
	    result))))

(defun distance-mapping-threshold-1 (point other-points threshold)
  (let (result)
    (dolist (other-point other-points (cons point (nreverse result)))
      (let ((distance (distance-between-points point other-point)))
	(when (<= distance threshold)
	  (push (cons other-point distance) result))))))

(defun distance-mapping-threshold (points threshold)
  ;; Half the list as I don't want duplicates
  (let* ((boundary-index (floor (/ (length points) 2)))
	 (first-points (subseq points 0 boundary-index))
	 (other-points (subseq points boundary-index))
	 result)
    (dolist (first-point first-points (nreverse result))
      (push (distance-mapping-threshold-1 first-point other-points threshold) result))))
