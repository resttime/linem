(in-package #:linem)

(in-readtable :qtools)

(define-widget ui (QWidget)
  ((image :accessor image)
   (pen :accessor pen :initform (q+:make-qpen))
   (painter :accessor painter)
   (last-point :accessor last-point :initform (q+:make-qpoint))
   (drawing :accessor drawing :initform nil)))

(defmethod initialize-instance :after ((this ui) &key)
  (with-slots (image pen painter) this
    (new this)
    (q+:set-window-title this "LineM")
    (q+:set-fixed-size this 640 480)
    (setf image (q+:make-qimage 640 480 (q+:qimage.format_argb32)))
    (q+:fill image (q+:make-qcolor 240 224 214))
    (setf painter (q+:make-qpainter (image this)))
    (q+:set-brush pen (q+:make-qbrush (q+:make-qcolor 128 0 0) (q+:qt.solid-pattern)))
    (q+:set-width pen 2)
    (q+:set-style pen (q+:qt.solid-line))
    (q+:set-cap-style pen (q+:qt.round-cap))
    (q+:set-join-style pen (q+:qt.round-join))
    (q+:set-pen painter pen)))

(define-override (ui paint-event) (event)
  (with-finalizing ((painter (q+:make-qpainter ui)))
    (q+:draw-image painter (q+:make-qpoint 0 0) (image ui))
    (q+:set-fixed-size ui 640 480)))

(define-override (ui mouse-press-event) (event)
  (when (enum-equal (q+:qt.left-button) (q+:button event))
    (with-slots (last-point drawing) ui
      (q+:set-x last-point (q+:x (q+:pos event)))
      (q+:set-y last-point (q+:y (q+:pos event)))
      (setf drawing t))))

(define-override (ui mouse-move-event) (event)
  (when (drawing ui)
    (with-slots (last-point painter) ui
      (q+:draw-line painter last-point (q+:pos event))
      (q+:set-x last-point (q+:x (q+:pos event)))
      (q+:set-y last-point (q+:y (q+:pos event)))
      (q+:update ui))))

(define-override (ui mouse-release-event) (event)
  (when (enum-equal (q+:qt.left-button) (q+:button event))
    (setf (drawing ui) nil)))

(defun start ()
;  (with-main-window (window 'ui :name "LineM"))
  (make-qapplication)
  (let ((window (make-instance 'ui)))
    (q+:show window)
    (q+:exec *qapplication*)
    (finalize window)
    (trivial-garbage:gc :full t)))
