;;; -*- Mode: Common-LISP -*-
;;;
;;; A simple DEFSYSTEM facility for the Common LISP MRS.
;;;
;;; AJY 2014-07-29.

(defun defsystem (sysname
		  &rest filelist)
  (setf (get sysname :defsystem) '())
  (loop for filename in filelist
       do
       (setf (get sysname :defsystem)
	     (append (get sysname :defsystem)
		     (list filename)))))

(defun load-system (sysname)
  (let ((filelist (get sysname :defsystem)))
    (loop for filename in filelist
	 do
	 (load filename))))

(defsystem 'MRS
    "TOP.LSP"
    "PLIST.LSP"
    "RESOLU.LSP")
