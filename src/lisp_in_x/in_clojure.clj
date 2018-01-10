(ns lisp-in-x.in-clojure
  (:require [clojure.walk :as walk]
            [clojure.core :as clj])
  (:refer-clojure :exclude [eval list cons apply load-file])
  (:import (java.io Writer)))

(declare eval)
(declare get-global)

(defrecord Cons [car cdr])

(defmethod print-method Cons
  [^Cons c ^Writer w]
  (do (.write w "(")
      (loop [^Cons c c]
        (cond
          (nil? c)
          (.write w ")")

          (nil? (.-cdr c))
          (do (.write w (format "%s)" (pr-str (.-car c)))))

          (instance? Cons (.-cdr c))
          (do (.write w (format "%s " (pr-str (.-car c))))
              (recur (.-cdr c)))

          :else
          (.write w (format "%s . %s)" (pr-str (.-car c)) (pr-str (.-cdr c))))))))


(defn cons
  ([v]
   (->Cons v nil))
  ([car cdr]
   (->Cons car cdr)))

(defn car [^Cons c]
  (.-car c))

(defn cdr [^Cons c]
  (.-cdr c))

(defn cons? [x]
  (instance? Cons x))

(defn bind [env sym val]
  (->Cons (->Cons sym val) env))

(defn list [& args]
  (reduce
    (fn [tail head]
      (cons head tail))
    nil
    (reverse args)))

(defn lookup [env sym]
  (if env
    (let [head (car env)]
      (if (= (car head) sym)
        (cdr head)
        (recur (cdr env) sym)))
    (get-global sym)))


(defn new-env [& args]
  (reduce
    (fn [env [car cdr]]
      (bind env car cdr))
    nil
    (partition 2 args)))



(def globals (atom nil))


(defn map-clojure-syms [syms]
  (zipmap
    syms
    (map resolve syms)))

(defn def-global [sym val]
  (swap! globals assoc sym val))

(defn get-global [sym]
  (let [result (get @globals sym ::not-found)]
    (if (identical? result ::not-found)
      (throw (ex-info "Global not Found"
                      {:global sym :env (sort (keys @globals))}))
      result)))

(defn eval-list [env lst]
  (when lst
    (cons (eval env (car lst))
          (eval-list env (cdr lst)))))

(defn cons->list [s]
  (if s
    (conj (cons->list (cdr s)) (car s))
    '()))

(defn apply [f args]
  (clj/apply f (cons->list args)))

(defn eval-apply [env f args]
  (if (nil? f)
    (throw (ex-info "Canot apply values to nil"
                    {:args args})))
  (let [lst (eval-list env args)]
    (apply f lst)))

(defn make-lambda [env syms body]
  (fn lambda [& args]
    (let [env (loop [syms syms
                     args args
                     env env]
                (if syms
                  (recur (cdr syms)
                         (next args)
                         (bind env (car syms) (first args)))
                  env))]
      (eval env body))))

(defn eval-do [env form]
  (if (cdr form)
    (do
      (eval env (car form))
      (recur env (cdr form)))
    (eval env (car form))))

(defn vararg [f]
  (fn [& args]
    (f (clj/apply list args))))

(defn eval-sexpr [env sym form]
  (cond
    (= sym 'if) (if (eval env (car form))
                  (eval env (car (cdr form)))
                  (eval env (car (cdr (cdr form)))))
    (= sym 'do) (eval-do env form)
    (= sym 'def) (let [sym (car form)
                       val (eval env (car (cdr form)))]
                   (def-global sym val)
                   val)
    (= sym 'quote) (car form)
    (= sym 'fn) (let [args (car form)
                          body (cons 'do (cdr form))]
                      (make-lambda env args body))
    (= sym 'resolve) (get-global (eval env (car form)))
    (= sym 'cond) (loop [form form]
                    (let [pred-expr (car form)
                          body-expr (car (cdr form))
                          next-expr (cdr (cdr form))]
                      (if form
                        (if (eval env pred-expr)
                          (eval env body-expr)
                          (recur next-expr))
                        nil)))
    (= sym 'let) (let [binds (car form)
                       body (cdr form)
                       env (loop [env env
                                  binds binds]
                             (if (nil? binds)
                               env
                               (recur (bind env (car binds) (eval env (car (cdr binds))))
                                      (cdr (cdr binds)))))]
                   (eval-do env body))
    :else (eval-apply env (eval env sym) form)))

(defn eval [env form]
  (cond
    (cons? form) (if (symbol? (car form))
                   (eval-sexpr env (car form) (cdr form))
                   (eval-apply env (eval env (car form)) (cdr form)))

    (symbol? form) (lookup env form)
    :else form))

(defn clj->lisp [form]
  (walk/postwalk
    (fn [f]
      (if (sequential? f)
        (clj/apply list f)
        f))
    form))

(defmacro eval-forms [& forms]
  `(do (reset-globals)
       (last (for [form# (quote ~forms)]
               (do (println form#)
                   (eval nil (clj->lisp form#)))))))

(defn read-file [file]
  (clj->lisp (read-string (str "(do " (slurp file) ")"))))

(defn load-file [file]
  (eval nil (read-file file)))

(defn die [msg & args]
  (throw (ex-info msg (clj/apply hash-map args))))

(defn reset-globals []
  (reset! globals (map-clojure-syms
                    '#{println + * - / < > <= >= = dec inc car cdr cons cons? symbol? apply
                       load-file nil? vararg read-file die})))

(defn eval-file [file]
  ;; Hack but it's simple
  (reset-globals)
  (load-file file))


(comment

  (do (reset-globals)
      (eval nil (clj->lisp '(do (def square (lambda [x]
                                                    (* x x)))
                                (square 2)))))

  (eval (new-env 'f false 'r 2)
        (clj->lisp '(if f r 42)))

  (type (clj->lisp '(if f r 42)))

  (type)
  (list 1 2 3)

  (lookup (new-env 'f 1 'x 2) 'y)

  (eval-forms
    (def square
      (fn [x] (* x x)))
    (square 42 42))

  (eval-forms
    (def fib
      (fn [n]
        (if (<= n 1)
          n
          (+ (fib (dec n))
             (fib (- n 2))))))
    (fib 12))

  (eval-file "src/lisp_in_x/tests.clj")

  (eval-file "src/lisp_in_x/deeper.clj")

  )