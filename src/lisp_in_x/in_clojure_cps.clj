(ns lisp-in-x.in-clojure-cps)

; Lisp interpreter in the continuation passing style

(definterface IThunk
  (invoke [this]))

(defmacro thunk [& body]
  `(reify
     IThunk
     (invoke [this]
       ~@body)))

(defmacro call-k [k & args]
  `(thunk (k ~@args)))

(defn run-all [thk]
  (if (instance? IThunk thk)
    (recur (.invoke thk))
    thk))

; Our version of the CEK machine, fully immutable, will use the following format:
; C E G K
; Control - the program
; Environment - local assignments
; Globals - Global assignments
; K - The current continuation



(defn eval [])
