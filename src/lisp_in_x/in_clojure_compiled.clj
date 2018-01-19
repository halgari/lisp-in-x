(ns lisp-in-x.in-clojure-compiled
  (:refer-clojure :exclude [cons compile eval load-file list apply])
  (:require [clojure.core :as clj]
            [clojure.walk :as walk])
  (:import (java.io Writer)))

;; Simple lisp to Clojure compiler

(def eval-ns-name 'lisp-in-x.eval-ns)

(def eval-ns (create-ns eval-ns-name))

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

(defn ensure-defined [sym]
  (if (not (ns-resolve eval-ns sym))
    (intern eval-ns sym))
  sym)

(defn cons->list [s]
  (if s
    (conj (cons->list (cdr s)) (car s))
    '()))

(def direct-mapping? '#{if do})

(declare lisp->clj)


(defn list [& args]
  (reduce
    (fn [tail head]
      (cons head tail))
    nil
    (reverse args)))

(defn clj->lisp [form]
  (walk/postwalk
    (fn [f]
      (if (sequential? f)
        (clj/apply list f)
        f))
    form))


(defn sexpr->clj [env sym form]
  (cond
    (direct-mapping? sym) `(~sym ~@(map (partial lisp->clj env) form))

    (= sym 'cond) `(cond ~@(map (partial lisp->clj env) form))

    (= sym 'def) `(def ~(first form) ~(lisp->clj env (second form)))
    (= sym 'fn) (let [[args & body] form
                      new-env (into env args)]
                  `(fn [~@(cons->list args)] ~@(map (partial lisp->clj new-env) body)))

    (= sym 'quote) `(quote ~(first form))

    (= sym 'resolve) `(clj/resolve ~(lisp->clj env (first form)))

    (= sym 'let) (let [[bindings & body] form
                       pairs (partition 2 (cons->list bindings))
                       new-env (into env (map first pairs))]
                   `(let [~@(loop [out []
                                   [[bind expr] & remain] pairs
                                   env env]
                              (if bind
                                (recur (into out [bind (lisp->clj env expr)])
                                       remain
                                       (conj env bind))
                                out))]
                      ~@(map (partial lisp->clj new-env) body)))

    :else `(~(lisp->clj env sym)
             ~@(map (partial lisp->clj env) form))))

(defn lisp->clj [env form]
  (cond
    (cons? form) (if (symbol? (car form))
                   (sexpr->clj env (car form) (cons->list (cdr form)))
                   (map (partial lisp->clj env)
                        (cons->list form)))
    (symbol? form) (if (contains? env form)
                     form
                     (ensure-defined form))
    :else form))

(defn eval
  ([form]
   (eval #{} form))
  ([env form]
   (let [form `(fn [] ~(lisp->clj env form))
         f (try
             (binding [*ns* eval-ns]
               (clj/eval form))
             (catch Throwable ex
               (clojure.pprint/pprint form)
               (throw ex)))]
     (f))))

(defn vararg [f]
  (fn [& args]
    (f (clj/apply list args))))

(defn apply [f args]
  (clj/apply f (cons->list args)))

(def predefed-globals '#{println + * - / < > <= >= = dec inc car cdr cons cons? symbol? apply
                         load-file nil? vararg read-file die})

(defn reset-globals []
  (doseq [global predefed-globals]
    (let [var (intern eval-ns global)]
      (alter-var-root var (fn [_ n] n) (resolve global)))))





(defn read-file [file]
  (clj->lisp (read-string (str "(do " (slurp file) ")"))))

(defn load-file [file]
  (eval (read-file file)))

(defn die [msg & args]
  (throw (ex-info msg (clj/apply hash-map args))))

(defn eval-file [file]
  ;; Hack but it's simple
  (reset-globals)
  (load-file file))


(comment

  (reset-globals)
  (ns-publics eval-ns)

  (lisp->clj #{} (read-file "src/lisp_in_x/stdlib.clj")

             #_(do (def foo 42)
                   ))

  (eval (clj->lisp '(resolve 'cons)))

  (lisp->clj nil '(let [x (+ 1 2)] x))

  (ns-resolve eval-ns 'foo)

  (partition 2 '[x 1 y 2])

  (eval-file "src/lisp_in_x/tests.clj")
  (eval-file "src/lisp_in_x/deeper.clj")

  (slurp "src/lisp_in_x/tests.clj")

  (type ((eval `(fn [] ~(cons 1)))))
  (eval '1)

  (reset-globals)

  (eval '(do
           (load-file "src/lisp_in_x/stdlib.clj")
           (load-file "src/lisp_in_x/lisp_in_lisp.clj")
           (reset-globals)
           (eval nil 'eval)))

  )
