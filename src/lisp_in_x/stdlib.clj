

(def append-assoc
  (fn [lst k v]
    (cons (cons k v) lst)))

(def lookup-assoc
  (fn [lst k]
    (if (nil? lst)
      nil
      (if (= (car (car lst)) k)
        (cdr (car lst))
        (lookup-assoc (cdr lst) k)))))

(def lookup-assoc-or
  (fn [lst k f]
    (if (nil? lst)
      (f k)
      (if (= (car (car lst)) k)
        (cdr (car lst))
        (lookup-assoc-or (cdr lst) k f)))))

(def replace-assoc
  (fn [lst k v]
    (if (nil? lst)
      (cons (cons k v) nil)
      (if (= (car (car lst)) k)
        (cons (cons k v) (cdr lst))
        (cons (car lst)
              (replace-assoc (cdr lst) k v))))))