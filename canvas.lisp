(in-package #:linem)

(in-readtable :qtools)

(define-widget canvas (QWidget)
  ((painter :accessor painter :finalized t)
   (image :accessor image :finalized t)
   (pen :accessor pen :initform (q+:make-qpen) :finalized t)
   (primary-color :accessor primary-color :initform (q+:make-qcolor 128 0 0) :finalized t)
   (secondary-color :accessor secondary-color :initform (q+:make-qcolor 240 224 214) :finalized t)
   (x :accessor x :initform 0)
   (y :accessor y :initform 0)
   (size :accessor size :initform 2)
   (color-flag :accessor color-flag :initform t)
   (drawing :accessor drawing :initform nil)
   (width :initform 640 :initarg :width)
   (height :initform 480 :initarg :height)))

(defmethod initialize-instance :after ((this canvas) &key)
  (with-slots (image pen painter width height) this
    (q+:set-focus-policy this (q+:qt.strong-focus))
    (q+:set-fixed-size this width height)
    (setf image (q+:make-qimage width height (q+:qimage.format_argb32)))
    (q+:fill (image this) (secondary-color this))
    (setf painter (q+:make-qpainter image))
    (q+:set-brush pen (q+:make-qbrush (primary-color this) (q+:qt.solid-pattern)))
    (q+:set-width pen (size this))
    (q+:set-style pen (q+:qt.solid-line))
    (q+:set-cap-style pen (q+:qt.round-cap))
    (q+:set-join-style pen (q+:qt.round-join))
    (q+:set-pen painter pen)
    (q+:set-mouse-tracking this t)
    (with-finalizing ((cursor (q+:make-qcursor (q+:qt.blank-cursor))))
      (q+:set-cursor this cursor))))

(define-signal (canvas color-swapped) ())

(defmethod current-color ((this canvas))
  (if (color-flag this)
    (primary-color this)
    (secondary-color this)))

(defmethod swap-color ((this canvas))
  (with-slots (color-flag) this
    (if color-flag
	(setf color-flag nil)
	(setf color-flag t)))
  (q+:set-color (pen this) (current-color this))
  (q+:set-pen (painter this) (pen this))
  (signal! this (color-swapped)))

(defmethod set-pen-size ((canvas canvas) size)
  (setf (size canvas) size)
  (q+:set-width (pen canvas) (size canvas))
  (q+:set-pen (painter canvas) (pen canvas)))

(defmethod draw-cursor ((this canvas) painter)
  (with-slots (x y size) this
    (with-finalizing ((point (q+:make-qpointf x y)))
      (q+:draw-ellipse painter point (/ size 2d0) (/ size 2d0)))))

(defmacro key-case (key &body cases)
  (let ((k (gensym)))
    `(let ((,k ,key))
       (cond ,@(mapcar (lambda (case) `((= ,k ,(car case)) ,(cadr case))) cases)))))

(defmethod prompt-clear ((this canvas))
  (when (enum-equal (q+:qmessagebox.yes)
		    (q+:qmessagebox-question this
					     "Confirmation"
					     "Are you sure you want to clear the canvas?"
					     (+ (q+:qmessagebox.yes) (q+:qmessagebox.cancel))
					     (q+:qmessagebox.yes)))
    (q+:fill (image this) (secondary-color this))))

(defmethod prompt-save ((this canvas))
  (let ((file-name (q+:qfiledialog-get-save-file-name this
						      "Save File"
						      (q+:qdir-home-path)
						      "Images(*.png)")))
    (when (> (length file-name) 0)
      (q+:save (image this) (concatenate 'string file-name ".png") "png"))))

(define-override (canvas key-press-event) (event)
  (let ((key (q+:key event)))
    (key-case key
      ((q+:qt.key_s) (prompt-save canvas))
      ((q+:qt.key_c) (prompt-clear canvas))
      ((q+:qt.key_x) (swap-color canvas)))
    (when (and (> key (q+:qt.key_0)) (<= key (q+:qt.key_9)))
      (set-pen-size canvas (- key (q+:qt.key_0)))
      (q+:update canvas))))

(define-override (canvas wheel-event) (event)
  (let ((scroll (/ (q+:delta event) 120)))
    (unless (and (<= (size canvas) 1) (minusp scroll))
      (q+:set-width (pen canvas) (incf (size canvas) scroll))
      (q+:set-pen (painter canvas) (pen canvas))
      (q+:update canvas))))

(define-override (canvas mouse-press-event) (event)
  (when (enum-equal (q+:qt.left-button) (q+:button event))
    (q+:draw-point painter x y)
    (setf drawing t)
    (q+:update canvas)))

(define-override (canvas mouse-move-event) (event)
  (when (drawing canvas)
    (q+:draw-line painter x y (q+:x event) (q+:y event)))
  (setf x (q+:x (q+:pos event)))
  (setf y (q+:y (q+:pos event)))
  (q+:update canvas))

(define-override (canvas mouse-release-event) (event)
  (when (enum-equal (q+:qt.left-button) (q+:button event))
    (setf (drawing canvas) nil)))

(define-override (canvas paint-event) (event)
  (with-finalizing ((painter (q+:make-qpainter canvas)))
    (q+:draw-image painter (q+:make-qpoint 0 0) (image canvas))
    (draw-cursor canvas painter)))
