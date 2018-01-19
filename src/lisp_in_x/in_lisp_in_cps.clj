(load-file "src/lisp_in_x/stdlib.clj")

(def bind
  (fn [env sym val]
    (append-assoc env sym val)))

(def lookup
  (fn [globals env sym k]
    (k globals (lookup-assoc-or env sym
                                (fn [sym]
                                  (lookup-assoc-or globals sym
                                                   (fn [sym]
                                                     (die "Global not defined" 'sym sym))))))))

(def eval-list
  (fn [globals env lst k]
    (if lst
      (evalk (car lst) globals env
             (fn [globals v]
               (eval-list globals env (cdr lst)
                          (fn [rest]
                            (k (cons v rest))))))
      (k nil))))

(def eval-apply
  (fn [globals env f args k]
    (eval-list globals env args
               (fn [args-resolved]
                 (f globals env args-resolved k)))))

(def eval-do
  (fn [globals env args k]
    (if (cdr args)
      (evalk (car args) globals env
             (fn [globals _]
               (eval-do globals env (cdr args) k)))
      (evalk (car args) globals env k))))

(def eval-def
  (fn [globals env args k]
    (let [sym (car args)
          expr (car (cdr args))]
      (evalk expr globals env
             (fn [globals val]
               (k (replace-assoc globals sym val) val))))))

(def bind-args
  (fn [env syms args]
    (if syms
      (bind-args
        (bind env (car syms) (car args))
        (cdr syms)
        (cdr args))
      env)))

(def eval-fn
  (fn [globals env args k]
    (let [syms (car args)
          body (cdr args)]
      (k globals (fn [globals _ args k]
                   (let [new-env (bind-args env syms args)]
                     (eval-do globals new-env body k)))))))

(def eval-if
  (fn [globals env args k]
    (let [test (car args)
          then (car (cdr args))
          else (car (cdr (cdr args)))]
      (evalk test globals env
             (fn [globals val]
               (evalk (if val then else) globals env k))))))

(def eval-cond
  (fn [globals env val args k]
    (if val
      (evalk (car args) globals env k)
      (if (cdr args)
        (evalk (car (cdr args))  globals env
               (fn [globals val]
                 (eval-cond globals env val (cdr (cdr args)) k)))
        (die "No match for cond")))))

(def eval-let
  (fn [globals env sym val bindings body k]
    (let [new-env (bind env sym val)]
      (if bindings
        (evalk (car (cdr bindings)) globals new-env
               (fn [globals val]
                 (eval-let globals new-env (car bindings) val (cdr (cdr bindings)) body k)))
        (eval-do globals new-env body k)))))

(def eval-sexpr
  (fn [globals env sym args k]
    (cond
      (= sym 'do) (eval-do globals env args k)
      (= sym 'def) (eval-def globals env args k)
      (= sym 'fn) (eval-fn globals env args k)
      (= sym 'if) (eval-if globals env args k)
      (= sym 'cond) (evalk (car args) globals env
                           (fn [globals val]
                             (eval-cond globals env val (cdr args) k)))
      (= sym 'let) (let [bindings (car args)
                         body (cdr args)]
                     (evalk (car (cdr bindings)) globals env
                            (fn [globals val]
                              (eval-let globals env (car bindings) val (cdr (cdr bindings)) body k))))
      (= sym 'resolve) (evalk (car args) globals env
                              (fn [globals val]
                                (k globals (lookup-assoc globals val))))
      (= sym 'quote) (k globals (car args))
      true (evalk sym globals env
                  (fn [globals f]
                    (eval-apply globals env f args k))))))




(def evalk
  (fn [form globals env k]
    (cond
      (cons? form) (if (symbol? (car form))
                     (eval-sexpr globals env (car form) (cdr form) k)
                     (evalk (car form) globals env
                            (fn [globals f]
                              (eval-apply globals env f (cdr form) k))))
      (symbol? form) (lookup globals env form k)
      true (k globals form))))

(def builtins '[println + * - / < > <= >= = dec inc car cdr cons cons? symbol?
                load-file nil? load-file die read-file])

(def add-builtins
  (fn [globals l]
    (if l
      (let [sym (car l)
            f (resolve sym)
            fwrap (fn [globals env args k]
                    (k globals (apply f args)))]
        (add-builtins (replace-assoc globals sym fwrap) (cdr l)))
      globals)))

(def -apply
  (fn [globals env args k]
    ((car args) globals env (car (cdr args)) k)))

(def -load-file
  (fn [globals env args k]
    (evalk (read-file (car args))
           globals env k)))

(def -vararg
  (fn [globals env fargs k]
    (let [f (car fargs)]
      (k globals (fn [globals env args k]
                   (f globals env (cons args nil) k))))))

(def default-globals (replace-assoc (replace-assoc (replace-assoc (add-builtins nil builtins)
                                                                  'load-file -load-file)
                                                   'apply -apply)
                                    'vararg -vararg))

(def eval
  (fn [env form]
    (evalk form default-globals env (fn [globals val] val))))

(println "EVAL " (eval nil '(load-file "src/lisp_in_x/deeper.clj")))
