;; This code is in a .clj file not because it's Clojure but because we can reuse
;; syntax highlighting of IDEs and Editors

(load-file "src/lisp_in_x/stdlib.clj")

(def globals nil)

(def def-global
  (fn [sym val]
    (def globals (replace-assoc globals sym val))))

(def get-global
  (fn [sym]
    (lookup-assoc-or globals sym
                     (fn [sym2]
                       (die "Global not defined" 'sym sym 'sym2 sym2)))))

(def bind
  (fn [env sym val]
    (append-assoc env sym val)))

(def lookup
  (fn [env sym]
    (lookup-assoc-or env sym get-global)))

(def eval-list
  (fn [env lst]
    (if lst
      (cons (eval env (car lst))
            (eval-list env (cdr lst)))
      nil)))

(def eval-apply
  (fn [env f args]
    (apply f (eval-list env args))))

(def eval-do
  (fn [env form]
    (if (cdr form)
      (do
        (eval env (car form))
        (eval-do env (cdr form)))
      (eval env (car form)))))

(def eval-cond
  (fn [env form]
    (let [pred-expr (car form)
          body-expr (car (cdr form))
          next-expr (cdr (cdr form))]
      (if form
        (if (eval env pred-expr)
          (eval env body-expr)
          (eval-cond env next-expr))
        nil))))

(def bind-args
  (fn [env syms args]
    (if syms
      (bind-args
        (bind env (car syms) (car args))
        (cdr syms)
        (cdr args))
      env)))

(def make-lambda
  (fn [env syms body]
    (vararg
      (fn [args]
        (eval (bind-args env syms args) body)))))

(def bind-let
  (fn [env binds]
    (if (nil? binds)
      env
      (bind-let (bind env (car binds)
                      (eval env (car (cdr binds))))
                (cdr (cdr binds))))))

(def eval-sexpr
  (fn [env sym form]
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

      (= sym 'cond) (eval-cond env form)

      (= sym 'let) (let [env (bind-let env (car form))
                         body (cdr form)]
                     (eval-do env body))
      true (eval-apply env (eval env sym) form))))

(def eval
  (fn [env form]
    (cond
      (cons? form) (if (symbol? (car form))
                     (eval-sexpr env (car form) (cdr form))
                     (eval-apply env (eval env (car form)) (cdr form)))

      (symbol? form) (lookup env form)
      true form)))

(def builtins '[println + * - / < > <= >= = dec inc car cdr cons cons? symbol? apply
                load-file nil? vararg load-file die read-file])

(def add-globals
  (fn [l]
    (if l
      (do
        (def-global (car l) (resolve (car l)))
        (add-globals (cdr l)))
      'done)))


;; Don't want to override our load-file so we'll rename inside reset
(def load-file-inner
  (fn [file]
    (eval nil (read-file file))))

(def reset-globals
  (fn []
    (def globals nil)
    (add-globals builtins)
    (def-global 'load-file load-file-inner)))

