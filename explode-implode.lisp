;;; -*- Mode: Common-LISP -*-
;;;
;;; The MACLISP EXPLODE and IMPLODE functions.
;;;
;;; ------------------------------------------------------------
;;;
;;; AJY 2015-03-18
;;;
;;; ------------------------------------------------------------
;;;


(in-package mrs)


(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1 
                                      :initial-element c)))
       (symbol-name sym)))


;;; EXPLODEN.  Explode an integer.

(defun exploden (int)
  (explode (intern (format nil "~A" int))))


(defun implode (l)
  (let ((sym (make-array 1 :fill-pointer 0 :adjustable t)))
    (loop for i in l do (setf sym
			      (concatenate 'string
					   sym
					   (format nil "~A" i))))
    (intern sym)))


