(defun istree (seq)
  (cond ((null seq) t)
        ((= (length seq) 3) (every #'(lambda (p) p) (mapcar
                                                     #'(lambda (s) (if (listp s)
                                                                       (istree s)
                                                                     t))
                                                     seq)))
        (t nil)))
