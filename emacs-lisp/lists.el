(defun my-last (seq)
  (if (null (cdr seq))
      seq
    (my-last (cdr seq))))

(defun my-but-last (seq)
  (if (<= (length seq) 2)
      seq
    (my-but-last (cdr seq))))

(defun element-at (seq index)
  (if (<= index 0) (error "Index should be > 1"))
  (if (= index 1)
      (car seq)
    (element-at (cdr seq) (- index 1))))

(defun my-length (seq)
  (if (null seq)
      0
    (let ((len 1)
          (lst seq))
      (while (not (null (cdr lst)))
        (setq len (1+ len))
        (setq lst (cdr lst)))
      len)))

(defun -my-reverse (seq)
  (if (null seq) (error "Reverse of nil"))
  (if (null (cdr seq))
      (list (car seq))
    (append (my-reverse (cdr seq)) (list (car seq)))))

(defun my-reverse (seq)
  (cond
   ((null seq) (error "Reverse of nil"))
   ((null (cdr seq)) (list (car seq)))
   (t (append (my-reverse (cdr seq)) (list (car seq))))))

(defun -my-flatten (seq)
  (if (null seq)
      nil
    (if (listp (car seq))
        (append (my-flatten (car seq)) (my-flatten (cdr seq)))
      (append (list (car seq)) (my-flatten (cdr seq))))))

(defun my-flatten (seq)
  (cond
   ((null seq) nil)
   (t (if (listp (car seq))
          (append (my-flatten (car seq)) (my-flatten (cdr seq)))
        (append (list (car seq)) (my-flatten (cdr seq)))))))

(defun compress (seq)
  (if (< (my-length seq) 2)
      seq
    (let ((lst seq))
      (if (eq (car lst) (element-at lst 2))
          (compress (cdr lst))
        (append (list (car lst)) (compress (cdr lst)))))))

(defun pack (seq)
  (if (null seq)
      nil
    (let ((item (car seq))
          (res (list (car seq)))
          (lst (cdr seq)))
      (while (eq item (car lst))
        (setq res (append res (list item)))
        (setq lst (cdr lst)))
      `(,res ,@(pack lst)))))

(defun encode (seq)
  (mapcar (lambda (lst)
            (list (count (car lst) lst) (car lst)))
          (pack seq)))

(defun encode-modified (seq)
  (mapcar (lambda (lst)
            (cond ((eq (car lst) 1) (cadr lst))
                  (t lst)))
          (encode seq)))

(defun decode (seq)
  (let ((res '()))
    (mapcar (lambda (lst)
              (dotimes (i (car lst))
                (setq res (append res (list (cadr lst))))))
            seq)
    res))

;; (defun my-last (seq)
;;   (car (last seq)))

(defun my-head (seq)
  (reverse (cdr (reverse seq))))

(defun dupli (seq)
  (my-flatten (mapcar (lambda (item) (list item item)) seq)))

(defun repli (seq n)
  (my-flatten
   (mapcar (lambda (item)
             (let ((res ()))
               (dotimes (i n)
                 (setq res (append res (list item))))
               res))
           seq)))

(defun drop-every (seq n)
  (let ((i 1)
        (res ()))
    (dotimes (i (my-length seq))
      (setq i (1+ i))
      (if (eq (mod i n) 0)
          nil
        (setq res (append res (list (element-at seq i))))))
    res))

(defun take (seq n)
  (if (>= n (my-length seq))
      seq
    (let ((res ()))
      (dotimes (i n res)
        (setq res (append res (list (element-at seq (1+ i)))))))))

(defun drop (seq n)
  (reverse (take (reverse seq) (- (my-length seq) n))))

(defun split (seq n)
  (if (>= n (my-length seq))
      `(,seq ())
    `(,(take seq n) ,(drop seq n))))

(defun slice (seq start stop)
  (take (drop seq start) (- stop start)))

(defun rotate (seq n)
  (append
   (drop seq n)
   (take seq n)))

(defun remove-at (seq pos)
  (append
   (take seq (1- pos))
   (drop seq pos)))

(defun insert-at (item seq pos)
  (append
   (take seq (1- pos))
   (list item)
   (drop seq pos)))

(cl-defun range (start stop &optional (inclusive t))
  (unless (null inclusive) (setq stop (1+ stop)))
  (if (> start stop) (error "Start should be < than stop"))
  (let ((res ()))
    (dotimes (i (- stop start) res)
      (setq res (append res (list (+ i start)))))))

(defun rnd-select (seq n)
  (let ((lst seq)
        (res ()))
    (if (> n (my-length seq)) (error "n must be <= than length"))
    (while (> n 0)
      (let* ((len (my-length lst))
            (pos (1+ (random len))))
        (setq res (append res (list (element-at lst pos))))
        (setq lst (remove-at lst pos))
        (setq n (1- n))))
    res))

(defun lotto-select (n stop)
  (rnd-select (range 1 stop) n))

(defun rnd-permu (seq)
  (let ((len (my-length seq)))
    (rnd-select seq len)))

(defun combination (n seq)
  (if (= n 1)
      (mapcar #'list seq)
    (let ((res ())
          (k 0))
      (dolist (i seq)
        (setq k (1+ k))
        (dolist (j `(,@(combination (1- n) (drop seq k))))
          (setq res `(,@res ,(append (list i) j)))))
      res)))

(defun generic-sort (seq cb)
  (if (null seq)
      nil
    (let* ((first (car seq))
           (rest (drop seq 1))
           (prim (funcall cb first))
           (smaller (-filter #'(lambda (l) (and (not (null l)) (> prim (funcall cb l)))) rest))
           (greater (-filter #'(lambda (l) (and (not (null l)) (<= prim (funcall cb l)))) rest)))
      `(,@(generic-sort smaller cb) ,first ,@(generic-sort greater cb)))))

(defun lsort (seq)
  (generic-sort seq #'my-length))

(defun freq (seq item cb)
  (let ((res 0))
    (dolist (i seq)
      (if (= (funcall cb i) (funcall cb item)) (setq res (1+ res))))
    res))

(defun lfsort (seq)
  (generic-sort seq #'(lambda (x) (freq seq x #'my-length))))


(provide 'lists)

