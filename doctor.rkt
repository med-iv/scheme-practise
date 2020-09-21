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

;2-5
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

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response history-replicas)
      (case (random (if (equal? null history-replicas) 2 3)) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
          ((2) (history-answer history-replicas))  ; 3й способ
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
; сделать assoc итеративным
; https://stackoverflow.com/questions/16221336/error-with-define-in-racket
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