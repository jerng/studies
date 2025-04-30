(defun main ()
        (format t "hello"))
(sb-ext:save-lisp-and-die "hello" :executable t :toplevel #'main)
