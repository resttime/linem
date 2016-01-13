(in-package #:linem)

(in-readtable :qtools)

(defconstant +golden-ratio+ 1.618034)

(define-widget main-window (QWidget)
  ((image-width :accessor image-width :initarg :image-width :initform 640)
   (image-height :accessor image-height :initarg :image-height :initform 480)))

(define-subwidget (main-window canvas) (make-instance 'canvas
						      :width (image-width main-window)
						      :height (image-height main-window)))

(define-subwidget (main-window color-button) (q+:make-qpushbutton)
  (q+:set-style-sheet color-button "background-color:rgb(128,0,0)")
  (q+:set-fixed-size color-button 50 50))

(define-slot (main-window color-button) ()
  (declare (connected color-button (pressed)))
  (swap-color canvas))

(define-slot (main-window change-color-button) ()
  (declare (connected canvas (color-swapped)))
  (if (color-flag canvas)
    (q+:set-style-sheet color-button "background-color:rgb(128,0,0)")
    (q+:set-style-sheet color-button "background-color:rgb(240,224,214)")))

(define-subwidget (main-window save-button) (q+:make-qpushbutton "Save"))
 
(define-subwidget (main-window clear-button) (q+:make-qpushbutton "Clear"))

(define-slot (main-window save-button) ()
  (declare (connected save-button (pressed)))
  (prompt-save canvas))

(define-slot (main-window clear-button) ()
  (declare (connected clear-button (pressed)))
  (prompt-clear canvas))

(define-subwidget (main-window side-layout) (q+:make-qvboxlayout)
  (q+:add-widget side-layout color-button)
  (q+:set-alignment side-layout color-button (q+:qt.align-center))
  (q+:add-widget side-layout save-button)
  (q+:add-widget side-layout clear-button))

(define-subwidget (main-window main-layout) (q+:make-qhboxlayout main-window)
  (q+:add-widget main-layout canvas)
  (q+:add-layout main-layout side-layout))

(define-override (main-window close-event) (event)
  (when (enum-equal (q+:qmessagebox.no)
		    (q+:qmessagebox-question main-window
					     "Confirmation"
					     "Are you sure you want to exit LineM?"
					     (+ (q+:qmessagebox.yes) (q+:qmessagebox.No))
					     (q+:qmessagebox.no)))
    (q+:ignore event)))

(defmethod initialize-instance :after ((this main-window) &key)
  (q+:set-window-title this "LineM")
  (q+:show this)
  (q+:set-fixed-width this (q+:width this))
  (q+:set-fixed-height this (q+:height this)))

(defun start (&optional (width 640) (height 480))
  (with-main-window (window (make-instance 'main-window
					   :image-width width
					   :image-height height))))
