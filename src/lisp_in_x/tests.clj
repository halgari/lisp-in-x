;; Not Clojure, but .clj for use with Clojure IDEs

(println "Loading Stdlib")
(load-file "src/lisp_in_x/stdlib.clj")

;; Test some math
(println "Math")
(def fib
  (fn [n]
    (if (<= n 1)
      n
      (+ (fib (dec n))
         (fib (- n 2))))))

(def fibs
  (fn [max n]
    (if (< n max)
      (do (println "Fib:" n " -> " (fib n))
          (fibs max (inc n)))
      'done)))

(fibs 10 0)

(println "Cons Interaction")
(println "Car List" (= (car '(1 2)) 1))
(println "Car Cdr List" (= (car (cdr '(1 2))) 2))
(println "Cons nil" (= (car (cons 42 nil)) 42))
(println "Cons Pair test" (= (cdr (cons 1 2)) 2))

(println "Assoc List interaction")
(def env (append-assoc (append-assoc nil 'x 42) 'y 3))
(println "Find x" (lookup-assoc env 'x))
(println "Find y" (lookup-assoc env 'y))
(println "Find z" (lookup-assoc env 'z))

(println "Test Cond")
(cond
  (= 1 1) (println "One"))
(cond
  (= 0 1) (println "One")
  (= 2 2) (println "Two"))

(println "TEST Let")
(println "1 = " (let [x 1] x))
(println "2 = " (let [x 2 y 1] x))
(println "3 = " (let [x 1 y 2 z 3] z))

(println "TEST sexpr apply")
(println "42 = " ((fn [x] x) 42))

(println "TEST vararg")
(println "vararg car 1 = " ((vararg (fn [args] (car args))) 1 2))
(println "vararg car cdr 2 = " ((vararg (fn [args] (car (cdr args)))) 1 2))


(load-file "src/lisp_in_x/lisp_in_lisp.clj")
(println "EVAL 42" (eval nil 42))
(println "EVAL (if true 1 0)" (eval nil '(if true 1 0)))
(println "EVAL (if false 1 0)" (eval nil '(if false 1 0)))
(println "EVAL (do 0 1)" (eval nil '(do 0 1)))
(println "EVAL (do 0)" (eval nil '(do 0)))
(println "EVAL (do (def foo 42) foo)" (eval nil '(do (def foo 42) foo)))

(println "EVAL ((fn [x] x) 42)" (eval nil '((fn [x] x) 42)))
(println "EVAL (let [x 42] x)" (eval nil '(let [x 42] x)))


(println "Resolve")
(def x 42)
(println "42 = " (let [z 'x] (resolve z)))

(println "Globals system")
(def-global 'z 44)
(println "z = 44 =" (get-global 'z))

(println "RESET GLOBALS")
(reset-globals)

(println "READ FILE")
(read-file "src/lisp_in_x/stdlib.clj")

(println "EVAL 4 * 4 = " (eval nil '(* 4 4)))

(println "EVAL square 4 * 4 = " (eval nil '(do (def square
                                                 (fn [n] (* n n)))
                                               (square 4))))

(println "EVAL (fibs 6) = " (eval nil '(do (def fib
                                             (fn [n]
                                               (if (<= n 1)
                                                 n
                                                 (+ (fib (dec n))
                                                    (fib (- n 2))))))
                                           (fib 6))))

(println "EVAL COND")
(eval nil '(cond
             (= 1 1) (println "One")))
(eval nil '(cond
            (= 0 1) (println "One")
            (= 2 2) (println "Two")))


