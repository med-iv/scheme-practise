#lang scheme/base
; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента

(require racket/string)
(require racket/format)
(require racket/set)

; множество знаков пунктуации
(define punct (set "," ";" ":"))

(define (ask-patient-name)
 (begin
  (printf "next!\n")
  (printf "who are you?\n")
  (print '**)
  (read-line)))

;2-5 добавил стоп-слово и количество пациентов
(define (visit-doctor stopword count)
   (let loop ((name (ask-patient-name))
              (count count))
       (if (or (= count 0) (equal? name stopword)) (printf "time to go home\n")
           (let ()
            (printf "hello, ~a\n" name)
            (printf "what seems to be the trouble\n")
            (doctor-driver-loop name null)
            (loop (if (= count 1) name (ask-patient-name)) (- count 1))
             ))))

;spring
(define (join-lst lst)
  ; \\1y меняем первое вхождение на y
  (regexp-replace* #rx" ([,;:.\\!\\?])" (string-join lst) "\\1"))

;spring
(define (lint user-response)
  (let* ((sent (car (string-split user-response #rx"[\\.\\?!]")))
         (letters (string-split sent ""))
         (enchance-letters (map (lambda (x) (if (set-member? punct x) (string-append " " x) x)) letters))
         (proc_list (string-split (string-join enchance-letters "") " "))
         )
    proc_list))
 

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name history-replicas)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read-line)))
      (cond 
	    ((equal? user-response "goodbye") ; реплика "goodbye" служит для выхода из цикла
             (printf "goodbye, ~a\n" name)
             (printf "see you next week\n"))
            (else (let ((prep (lint user-response))) 
             (printf (join-lst (reply prep history-replicas))) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons (change-person prep) history-replicas))
             )))))

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response history-replicas)
  (let* ((avail-stratigies (filter (lambda (strat) ((predicate strat) user-response history-replicas)) strategies))
         (wght-strategies (map cdr avail-stratigies))
        )
    ((weighted-random wght-strategies) user-response history-replicas)))
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response history-replicas)
        (cons (pick-random '("you seem to think that"
                               "you feel that"
                               "why do you believe that"
                               "why do you say that"
                               ; 1-1 добавил реплики
                               "why do you think that"
                               "you believe that"
                               "you think that")
                )
                (change-person user-response)
        )
 )

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace3 '(("am" "are")
                        ("are" "am")
                        ("i" "you")
                        ("me" "you")
                        ("mine" "yours")
                        ("my" "your")
			("myself" "yourself")
                        ("you" "i")
                        ("your" "my")
                        ("yours" "mine")
			("yourself" "myself"))
                      phrase)
 )
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                        (car lst) ; иначе в начале ответа помещается начало списка без изменений
                              )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )))


; 1-2 новая реализация many-replace
(define (many-replace2 replacement-pairs lst)
        (let loop ((lst lst) (result null))
          (cond ((null? lst) (reverse result))
                (else (loop (cdr lst) (cons (let ((pat-rep (assoc (car lst) replacement-pairs)))
                                              (if pat-rep (cadr pat-rep)
                                                          (car lst))
                                              )
                                            result)
                    
                        )
                  )
            )
          )
  )

; 1-3 новая реализация many-replace
(define (many-replace3 replacement-pairs lst)
  (map (lambda (x)(let ((pat-rep (assoc x replacement-pairs)))
                   (if pat-rep (cadr pat-rep) x)
                    )
         )
        lst)
  )

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge user-response history-replicas)
       (pick-random '(("please go on")
                       ("many people have the same sorts of feelings")
                       ("many of my patients have told me the same thing")
                       ; 1-1 добавил реплики
                       ("please continue")
                       ("it is ok")
                       ("it is normal nowadays")
                       )
         )
)

; 3й способ генерации ответной реплики -- earlier you said + вспоминаем старую фразу
(define (history-answer user-response history-replicas)
  (cons "earlier you said that" (list-ref history-replicas (random (length history-replicas)))
    )
  )

;2-6
(define keywords
'(( ; начало данных 1й группы
    ("depressed" "suicide" "exams" "university") ; список ключевых слов 1й группы
    ( ; список шаблонов для составления ответных реплик 1й группы 
	  ("when you feel depressed, go out for ice cream")
      ("depression is a disease that can be treated")
	)
  ) ; завершение данных 1й группы
  ( ; начало данных 2й группы ...
    ("mother" "father" "parents" "brother" "sister" "uncle" "ant" "grandma" "grandpa")
       (
	  ("tell me more about your * , i want to know all about your *")
          ("why do you feel that way about your * ?")
          ("are you proud of your family?")
          ("do you love your * ?")
       )
  )
  (
    ("university" "scheme" "lections")
	(
	  ("your education is important")
	  ("how many time do you spend to learning ?")
          ("you are smart")
          ("are you studying now ?")
	)
  )
  ( ; 3-я группа
    ("school" "university" "club")
       (
         ("do you have friends from * ?")
         ("do you like your * ?")
       )
  )
  ( ; 4-я группа
    ("club" "bar" "cafe" "restaurant")
       (
          ("what is your favorite dish ?")
          ("what is your favorite drink ?")
          ("what is your favorite * ?")
       )
  )
))

; список ключей
(define keys
  (foldl append '() (map car keywords))
  )

; взвешенный рандом, на вход подается список из пар, первый элемент пары - значение, второй - вес
; (weighted-random '((1 0) (2 1)))
(define (weighted-random wght-lst)
  (define (subs-pick random-n wghts index)
    (if (not (positive? random-n)) index
        (subs-pick (- random-n (car wghts)) (cdr wghts) (+ index 1))
        )
    )
  (let* ((wghts (map cadr wght-lst))
         (sum-w (foldl + 0 wghts))
          (random-n (+ 1 (random sum-w)))
          (index (subs-pick (- random-n (car wghts)) (cdr wghts) 0))
          )
     (list-ref (map car wght-lst) index)
    )
  )

;функция возвращает список из ключевых слов из ответа пользователя
(define (get-user-keywords user-response)
  (let* ((is-pres? (lambda (elem) (member elem keys)))
         (res (filter is-pres? user-response))
         )
    res
    )
  )

;функция возвращает список реплик для данного ключа
(define (values key)
  (let* ((is-pres? (lambda (elem) (member key (car elem))))
         (vals (map cdr (filter is-pres? keywords)))
         )
    (map caar vals)
    )
  )
               
; 4-ая стратегия
(define (keywords-answer user-response history-replicas)
  (let* ((user-keywords (get-user-keywords user-response))
         (key (pick-random user-keywords))
         (template (pick-random (values key)))
         (answer (many-replace3 '((* key)) template))
         )
     answer
    )
  )

; есть ли ключевое слово
(define (is-keys? user-response history-replicas)
  (ormap (lambda (elem)
           (ormap (lambda (x) (if (equal? x elem) x #f)) keys)
           ) user-response)
  )

;были ли у пользователя репликиы
(define (is-replicas? user-response history-replicas)
  (equal? null history-replicas)
  )


;список стратегий с функциями-предикатами и весами
(define strategies (list (list (lambda (a1 a2) #t) qualifier-answer 1)
                         (list (lambda (a1 a2) #t) hedge 1)
                         (list is-keys? keywords-answer 1)
                         (list is-replicas? history-answer 1)
                         )
  )
(define (predicate str) (list-ref str 0))


;(visit-doctor "goodbye" 3)