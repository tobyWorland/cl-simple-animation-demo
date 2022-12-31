
(defpackage :animation
  (:use :cl)
  (:export :run)
  (:import-from :sdl2 :set-render-draw-color :render-clear
   :make-rect :render-fill-rect :free-rect :set-window-title
   :get-ticks :delay))
