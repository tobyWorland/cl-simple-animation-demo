
(defsystem "animation"
  :depends-on (:sdl2)
  :serial t
  :components ((:file "package")
	       (:file "misc")
	       (:file "point")
	       (:file "render")))
