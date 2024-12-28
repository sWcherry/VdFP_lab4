<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>

<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
з дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"><b>Студентка</b>: Панченко Вікторія Володимирівна КВ-12</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання

Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3 з такими змінами:
    - використати функції вищого порядку для роботи з послідовностями (де це доречно);
    - додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: `key` та `test`, що працюють аналогічно до того, як працюють параметри з такими назвами в функціях, що працюють з послідовностями. При цьому `key` має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за можливості, має бути мінімізоване.


## Варіант першої частини 8

Алгоритм сортування обміном №4 ("шейкерне сортування") за незменшенням.

## Лістинг реалізації першої частини завдання

```lisp
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
              (right-left (reverse (subseq new-lst L (1+ new-R))) 
                          (1- new-R) new-R nil)
            (functional (append (subseq new-lst 0 L)
                                new-middle
                                (nthcdr (1+ new-R) new-lst))
                        new-L new-R))))))
```

### Тестові набори та утиліти першої частини

```lisp
(defun check-functional (name lst expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (functional lst) expected)
          name))

(defun test-functional ()
  (check-functional "test1" '(0.3 5 0 -1 -3 -3 5.1) '(-3 -3 -1 0 0.3 5 5.1))
  (check-functional "test2" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-functional "test3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-functional "test4" '(2 2 2 2 0) '(0 2 2 2 2)))
```

### Тестування першої частини

```lisp
CL-USER> (test-functional)
passed test1
passed test2
passed test3
passed test4
NIL
```

## Варіант другої частини 4

Написати функцію `add-next-reducer` , яка має один ключовий параметр — функцію `transform` . `add-next-reducer` має повернути функцію, яка при застосуванні в якості першого аргументу `reduce` робить наступне: кожен елемент списку-аргументу `reduce` перетворюється на точкову пару, де в комірці CAR знаходиться значення поточного елемента, а в комірці CDR знаходиться значення наступного елемента списку (тобто того, що знаходиться "справа"). Якщо функція `transform` передана, тоді значення поточного і наступного елементів, що потраплять у результат, мають бути змінені згідно `transform` . Обмеження, які накладаються на використання функції-результату `add-next-reducer` при передачі у `reduce` визначаються розробником (тобто, наприклад, необхідно чітко визначити, якими мають бути значення ключових параметрів функції `reduce` `from-end` та `initial-value` ). `transform` має виконатись мінімальну кількість разів.

```lisp
CL-USER> (reduce (add-next-reducer)
         '(1 2 3)
         :from-end ...
         :initial-value ...)
((1 . 2) (2 . 3) (3 . NIL))
CL-USER> (reduce (add-next-reducer :transform #'1+)
         '(1 2 3)
         :from-end ...
         :initial-value ...)
((2 . 3) (3 . 4) (4 . NIL))
```

## Лістинг реалізації другої частини завдання

```lisp
(defun add-next-reducer (&key transform)
  (lambda (elem acc)
    (if transform
        (cons (cons (funcall transform elem) (caar acc)) acc)
        (cons (cons elem (caar acc)) acc))))
```

### Тестові набори та утиліти другої частини

```lisp
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
```

### Тестування другої частини

```lisp
CL-USER> (test-add-next-reducer)
passed test1
passed test2
passed test3
passed test4
passed test5
NIL
```
