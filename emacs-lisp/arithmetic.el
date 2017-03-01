(load-file "./lists.el")

(defun is-prime (x)

  (let ((res (cond ((<= x 1) 0)
          ((<= x 3) 1)
          ((or (= (mod x 2) 0) (= (mod x 3) 0)) 0)
          (t (let ((s (1+ (sqrt x))))
               (dolist (i (drop-every (range 3 s) 2))
                 (if (= (mod x i) 0) 0)))))))
    (if (null res)
        t
      (cond ((= res 1) t)
            ((= res 0) nil)))))

(defun gcd (x y)
  (if (= y 0)
      x
    (gcd y (mod x y))))

(defun coprime (x y)
  (= (gcd x y) 1))

(defun totient-phi (x)
  (let ((res 0))
    (dotimes (i (- x 1))
      (if (coprime x (1+ i))
          (setq res (1+ res))))
    res))

(defun prime-factors (x)
  (let ((res ()))
    (dotimes (i (sqrt x))
      (setq i (1+ i))
      (if (and (= 0 (mod x i)) (is-prime i))
          (setq res (append res (list i)))))
    res))
