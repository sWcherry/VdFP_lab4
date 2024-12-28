(defun left-right (lst i k middle &key (key #'identity) (test #'>))
  (if (cdr lst)
      (let ((current (funcall key (car lst)))
            (next (funcall key (second lst))))
        (if (funcall test current next)
            (left-right (cons current (cddr lst))
                        (1+ i) i
                        (cons next middle))
            (left-right (cdr lst) (1+ i) k
                        (cons current middle))))
      (values k (append (reverse middle) lst))))

(defun right-left (lst i k middle &key (key #'identity) (test #'>))
  (if (cdr lst)
      (let ((current (funcall key (car lst)))
            (next (funcall key (second lst))))
        (if (funcall test next current)
            (right-left (cons current (cddr lst))
                        (1- i) i
                        (cons next middle))
            (right-left (cdr lst) (1- i) k
                        (cons current middle))))
      (values (1+ k) (append lst middle))))

(defun functional (lst &optional (L 0) (R (1- (list-length lst))))
  (if (>= L R)
      lst
      (multiple-value-bind (new-R new-middle)
          (left-right (subseq lst L (1+ R)) L L nil)
        (let ((new-lst (append (subseq lst 0 L)
                               new-middle
                               (nthcdr (1+ R) lst))))
          (multiple-value-bind (new-L new-middle)
              (right-left (reverse (subseq new-lst L (1+ new-R))) (1- new-R) new-R nil)
            (functional (append (subseq new-lst 0 L)
                                new-middle
                                (nthcdr (1+ new-R) new-lst))
                        new-L new-R))))))

(defun check-functional (name lst expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (functional lst) expected)
          name))

(defun test-functional ()
  (check-functional "test1" '(0.3 5 0 -1 -3 -3 5.1) '(-3 -3 -1 0 0.3 5 5.1))
  (check-functional "test2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-functional "test3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-functional "test4" '(2 2 2 2 0) '(0 2 2 2 2)))
