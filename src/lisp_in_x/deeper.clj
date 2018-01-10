;; NOT CLOJURE - just lisp

;; Bit of a fun exercise...write lisp in lisp in lisp until the program blows up

(println "MUST GO DEEPER!")
(load-file "src/lisp_in_x/lisp_in_lisp.clj")
(reset-globals)
(eval nil '(load-file "src/lisp_in_x/deeper.clj"))