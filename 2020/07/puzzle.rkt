#lang racket

(require racket/file)

(define (bag-to-hash s)
  (let*
      ((parts (string-split s " "))
       (n (string-trim (car parts)))
       (bag (string-join (list (list-ref parts 1) (list-ref parts 2)))))
    `(,bag ,n)))

(define (parse-contents contents)
  (let ((bags (string-split contents ",")))
    (make-hash (map bag-to-hash bags))))

(define (parse-line line)
  (let ((parts (string-split line " bags contain ")))
    (cond
      [(null? parts) '()]
      [else
       (let ((bag-name (list-ref parts 0))
             (contents (parse-contents (list-ref parts 1))))
         (cond
           [(equal? (list-ref parts 1) "no other bags.") `(,bag-name, '())]
           [else `(,bag-name ,contents)]))])))

;; This will be very inefficient :)
(define (can-hold-shiny-gold-count m)
  (letrec
      ((can-hold?
        (lambda (bag)
          (let ((contents (car (hash-ref m bag))))
            (cond
              [(null? contents) #f]
              [else
               (ormap
                identity
                (hash-map contents
                 (lambda (inside n)
                  (cond
                    [(equal? inside "shiny gold") #t]
                    [else (can-hold? inside)]))))])))))
    (count
     identity
     (hash-map m
      (lambda (bag _contents)
        (can-hold? bag))))))

; again, not efficient nor tail-recursive :)
(define (bags-inside-shiny-gold-count m)
  (letrec
      ((bags-inside
        (lambda (bag)
          (let ((contents (car (hash-ref m bag))))
            (cond
              [(null? contents) 0]
              [else
               (apply
                +
                (hash-map contents
                 (lambda (inside n-str)
                   (let ((n (string->number (car n-str))))
                     (+ n (* n (bags-inside inside)))))))])))))
                
  (bags-inside "shiny gold")))

(define bag-map
  (let ((lines (file->lines "input")))
    (make-hash (map (lambda (line) (parse-line line)) lines))))

(define (part1)
  (can-hold-shiny-gold-count bag-map))

(define (part2)
  (bags-inside-shiny-gold-count bag-map))

(part1)
(part2)