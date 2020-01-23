
perm filename DEMO.LSP[MRS,LSP] blob sn#702096 filedate 1983-03-18 generic text, type T, neo UTF8


#+maclisp(defun $demo () (demo '|mrs.demo|))
#+franz(defun $demo () (demo '|mrs.demo|))
#+lispm(defun $demo () (demo '|mrs.demo|))

(defun demo (f)
  (terpri) (princ '|Type a carriage return to advance.|)
  (setq f (open f 'in))
  (do s (read f 'end) (read f 'end) (eq 'end s)
      (do c (tyi t) (tyi t) (= 13. c))
      (print s)
      (print (eval s)))
  (terpri)
  (close f)
  'done)

