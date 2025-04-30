- `apt get install sbcl` provided a working binary, but it couldn't properly
  run the minimal example from :
  https://susam.net/building-common-lisp-executables.html
```lisp
(defun main ()
  (format t "hello, world~%"))
(sb-ext:save-lisp-and-die "hello" :executable t :toplevel #'main)
```



- so now i am trying to : Download pre-compiled binaries here : https://www.sbcl.org/platform-table.html
