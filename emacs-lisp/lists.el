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
