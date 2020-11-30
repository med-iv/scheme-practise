#lang scheme/base
(require racket/string)
(require racket/format)
(require racket/set)
(require racket/vector)

(define N 5)
(define punct (set "," ";" ":" "." "!" "?"))
(define end_punct (set "." "!" "?" "$"))
;(define train (open-input-file "dummy_texts.txt"))
(define train_data "dummy_texts.txt")

(define dump_name "dump.txt")

(define direct-graph (make-hash))
(define reverse-graph (make-hash))

; делим предложение на токены, на вход подается одна строка
(define (lint sent)
  (let* ((letters (string-split sent ""))
         (enchance-letters (map (lambda (x) (if (set-member? punct x) (string-append " " x) x)) letters))
         (proc_list (string-split (string-join enchance-letters "") " ")))
    proc_list))

; считывает обучающую выборку, заполняет структуры
(define (read-file file_name)
  (let ((file (open-input-file train_data)))
    ; идем циклом по линиям
    (let loop-line ()
      (let* ((line (read-line file)))
        (if (eof-object? line) (void)
            ; получаем токены, список n-gram
            (let* ((linted-line (lint line))
                   (ngrams (gen-ngrams linted-line N))
                   (bigrams (gen-ngrams linted-line 2)))
              ; идем циклом по вектору n-gram, пустой список гарантированно не будет
              (let ngram-loop ((ngram (vector-ref ngrams 0))
                                (ngrams (vector-drop ngrams 1))
                                (is-bigrams #t)
                                (n N))
                (begin
                  ; заполняем прямой граф
                 (let* ((key-direct (vector-take ngram (- n 1)))
                        (value-direct (vector-ref ngram (- n 1)))
                        (vertex (hash-ref direct-graph key-direct (make-vector 0)))
                        (new-vertex (vector-append vertex (make-vector 1 (list value-direct 1)))))
                        (hash-set! direct-graph key-direct new-vertex))
                  ; заполняем обратный граф
                 (let* ((key-reverse (vector-drop ngram 1))
                        (value-reverse (vector-ref ngram 0))
                        (vertex (hash-ref reverse-graph key-reverse #(("$" 1))))
                        (new-vertex (vector-append vertex (make-vector 1 (list value-reverse 1)))))
                        (hash-set! reverse-graph key-reverse new-vertex))
                 ; условие продолжение цикла
                 (cond ((not (vector-empty? ngrams)) (ngram-loop (vector-ref ngrams 0) (vector-drop ngrams 1) is-bigrams n))
                       (is-bigrams (ngram-loop (vector-ref bigrams 0) (vector-drop bigrams 1) #f 2))
                       (else (loop-line))
                       )))))))))

; возвращает вектор, где каждый элемент вектора - n-грамма, каждая n-gramm - vector                 
(define (gen-ngrams linted-line n) 
  (let* ((sent (list->vector linted-line))
         (l (+ (- (vector-length sent) n) 1)))
    (if (< l 0) (void)
    (begin
      (let ((result (make-vector l)))
      (let loop ((iter (- l 1))
                 (sent sent))
        (if (< iter 0) null
        (begin
          (vector-set! result iter (vector-take sent n))
          (loop (- iter 1) (vector-drop sent 1))
          ))
        result))))))

; прямая стратегия
(define (direct-generator)
  (let* ((index (random (hash-count direct-graph)))
         (first-ngram (hash-iterate-key direct-graph index))
         (value (hash-ref direct-graph first-ngram))
         (word (weighted-random-vector (vector->list value))))
    (forward-loop first-ngram first-ngram word)))

; генератор вперед     
(define (forward-loop sentence
                     ngram
                     word)
      (if (set-member? end_punct word)
          (vector-append sentence (make-vector 1 word))
          (let* ((sentence (vector-append sentence (make-vector 1 word)))
                 (ngram (vector-append (vector-drop ngram 1) (make-vector 1 word)))
                 (new-value (hash-ref direct-graph ngram))
                 (new-word (weighted-random-vector (vector->list new-value))))
            (forward-loop sentence ngram new-word))))


; генератор назад     
(define (backward-loop sentence
                     ngram
                     word)
      (if (set-member? end_punct word)
          (sentence)
          (let* ((sentence (vector-append (make-vector 1 word) sentence))
                 (ngram (vector-append (make-vector 1 word) (vector-drop ngram 1)))
                 (new-value (hash-ref reverse-graph ngram))
                 (new-word (weighted-random-vector (vector->list new-value))))
            (backward-loop sentence ngram new-word))))


;взвешенный рандом для вектора
(define (weighted-random-vector wght-lst)
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
    ))

; смешанная стратегия, на вход поступает вектор предложения
(define (mixted-generator sentence)
  (let* ((index (random (- (vector-length sentence) N)))
         (first-ngram (vector-take (vector-drop sentence (- index 1)) N)))
    (begin
      ; здесь будет так, есть n-грамма - ок, иначе ищем биграмму - нет, тупой прямой генератор
      (cond ((
         
         

; создание дампа
(define (make-dump dump_name)
  (define dump (open-output-file dump_name #:exists 'replace))
  (write direct-graph dump)
  (write reverse-graph dump)
  (close-output-port dump))

; функция инициализации. prod - грузит дамп, train - обучается
(define (init mode)
  (if (equal? mode "prod")
      (load-dump dump_name)
      (begin
        (read-file train_data)
        (make-dump dump_name))))

; загрузка дампа
(define (load-dump dump_name)
  (define dump (open-input-file dump_name))
  (set! direct-graph (read dump))
  (set! reverse-graph (read dump))
  (close-input-port dump))

(define (test-func)
  (init "prod"))
(test-func)