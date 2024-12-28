(defun add-next-reducer (&key transform)
  (lambda (elem acc)
    (if transform
        (cons (cons (funcall transform elem) (caar acc)) acc)
        (cons (cons elem (caar acc)) acc))))

(defun check-add-next-reducer (name fun expected)
  (let ((result (reduce (add-next-reducer :transform fun)
                        '(1 2 3)
                        :from-end t
                        :initial-value nil)))
    (format t "~:[FAILED~;passed~] ~a~%"
            (equal result expected)
            name)))

(defun test-add-next-reducer ()
  (check-add-next-reducer "test1" nil '((1 . 2) (2 . 3) (3)))
  (check-add-next-reducer "test2" #'1+ '((2 . 3) (3 . 4) (4)))
  (check-add-next-reducer "test3" #'1- '((0 . 1) (1 . 2) (2)))
  (check-add-next-reducer "test4" #'- '((-1 . -2) (-2 . -3) (-3)))
  (check-add-next-reducer "test5" #'/ '((1 . 1/2) (1/2 . 1/3) (1/3))))
