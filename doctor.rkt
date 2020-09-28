; заготовка "Доктора". Август 2019
#lang scheme/base
; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
; убрать флаг из параметров


(define (ask-patient-name)
 (begin
  (println '(next!))
  (println '(who are you?))
  (print '**)
  (car (read))
 ) 
)

;2-5 добавил стоп-слово и количество пациентов
(define (visit-doctor stopword count)
  (if (equal? count 0) (print '(time to go home))
      (let ((name (ask-patient-name)))
       (if (equal? name stopword) (print '(time to go home))
           (let ()
            (printf "(hello, ~a)\n" name)
            (print '(what seems to be the trouble?))
            (doctor-driver-loop name null stopword count)
             )
         )
        )
    )
  )

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name history-replicas stopword count)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "(goodbye, ~a)\n" name)
             (print '(see you next week))
             (newline)
             (visit-doctor stopword (- count 1)))
            (else (print (reply user-response history-replicas)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons (change-person user-response) history-replicas) stopword count)
             )
       )
      )
)

(define (is-replicas history-replicas)
  (if (equal? null history-replicas) 0 1)
  )

(define (is-keys user-response)
  (if (is-keys? user-response keys) 1 0)
  )

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response history-replicas)
      ;(case (random (if (equal? null history-replicas) 3 4))
       (case (weighted-random '(3 2 0 1) (list (is-replicas history-replicas) (is-keys user-response) 1 1))
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
          ((2) (keywords-answer user-response))  ; 4й способ
          ((3) (history-answer history-replicas))  ; 3й способ
      )
   )
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random '((you seem to think that)
                               (you feel that)
                               (why do you believe that)
                               (why do you say that)
                               ; 1-1 добавил реплики
                               (why do you think that)
                               (you believe that)
                               (you think that))
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
        (many-replace3 '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
						(myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
						(yourself myself))
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
               )
         )
  )


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
(define (hedge)
       (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       ; 1-1 добавил реплики
                       (please continue)
                       (it is ok)
                       (it is normal nowadays)
                       )
         )
)

; 3й способ генерации ответной реплики -- earlier you said + вспоминаем старую фразу
(define (history-answer history-replicas)
  (append '(earlier you said that) (list-ref history-replicas (random (length history-replicas)))
    )
  )


;2-6

(define keywords
'(( ; начало данных 1й группы
    (depressed suicide exams university) ; список ключевых слов 1й группы
    ( ; список шаблонов для составления ответных реплик 1й группы 
	  (when you feel depressed, go out for ice cream)
      (depression is a disease that can be treated)
	)
  ) ; завершение данных 1й группы
  ( ; начало данных 2й группы ...
    (mother father parents brother sister uncle ant grandma grandpa)
    (
	  (tell me more about your * , i want to know all about your *)
      (why do you feel that way about your * ?)
	)
  )
  
  (
    (university scheme lections)
	(
	  (your education is important)
	  (how many time do you spend to learning ?)
	)
  )
  
  ( ; 3-я группа
    (school university club)
       (
         (do you have friends from * ?)
         (do you like your * ?)
       )
  )

  ( ; 4-я группа
    (club bar cafe restaurant)
       (
          (what is your favorite dish ?)
          (what is your favorite drink ?)
          (what is your favorite * ?)
       )
  )
))

(define (flatten x)
  (cond ((null? x) '())
        ((pair? x) (append (flatten (car x)) (flatten (cdr x))
                     )
          )
        (else (list x))
    )
  )

; есть ли ключевое слово
(define (is-keys? user-response keys)
  (ormap (lambda (elem)
           (ormap (lambda (x)
                     (if (equal? x elem) x
                         #f
                      )
                     ) keys)
           )
           user-response)
  )


; список ключей
(define keys
  (flatten (map (lambda (elem) (car elem)) keywords))
  )

; служебная функция
(define (subs-pick random-n wghts index)
  (if (not (positive? random-n)) index
      (subs-pick (- random-n (car wghts)) (cdr wghts) (+ index 1))
      )
  )

(define (weighted-random lst wghts)
  (let* ((sum-w (foldl + 0 wghts))
          (random-n (+ 1 (random sum-w)))
          (index (subs-pick (- random-n (car wghts)) (cdr wghts) 0))
          )
     (list-ref lst index)
    )
  )


(define (get-user-keywords user-response)
  (let* ((cur-keys keys)
         (is-pres? (lambda (elem) (is-keys? (list elem) cur-keys)))
         (res (filter is-pres? user-response))
         )
    res
    )
  )



         
(define (values key)
  (let* ((is-key? (lambda (elem) (is-keys? (car elem) (list key)))
           )
         (vals (map (lambda (elem) (cdr elem))
                             (filter is-key? keywords)
                       )
                     
          )
         )
    (map (lambda (x) (car (car x))) vals)
    )
  )

                  
; 4-ая стратегия
(define (keywords-answer user-response)
  (let* ((user-keywords (get-user-keywords user-response))
         (key (pick-random user-keywords))
         (template (pick-random (values key)))
         (answer (many-replace3 '((* key)) template))
         )
     answer
    )
  )


