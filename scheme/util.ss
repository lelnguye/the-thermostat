;; util.ss
; by tsip
;;
;; miscellaneous functions 

;; checks that all values in a list evaluates to true
(define (apply-and ls)
    (if (null? ls)
        #t
        (if (not (car ls))
            #f
            (apply-and (cdr ls)))))

;; creates a list of prime numbers from 
;; 2 to the given upper bound
(define (list-primes upper-bound)
    (let ((gen-list (lambda (n f) (if (= n upper-bound) '() (cons n (f (+ n 1) f)))))
          (remove-multiples-f 
            (lambda (ls n f)
                (if (null? ls ) 
                    '()
                    (let ((v (car ls)))
                        (if (and (not (= v n))
                                (= (mod v n) 0))
                            (f (cdr ls) n f)
                            (cons v (f (cdr ls) n f)))))))
          
          (get-primes-f
            (lambda (ls i remove-multiples-f f)
                (if (>= i (length ls))
                    ls
                    (let ((n (list-ref ls i))
                          (new-list (lambda (n) (remove-multiples-f ls n remove-multiples-f))))
                        (f (new-list n) (+ i 1) remove-multiples-f f))))))
        (get-primes-f (gen-list 2 gen-list) 0 remove-multiples-f get-primes-f)))


(define (index-of ls elem compare-f)
    (let ((loop-f
            (lambda (i f)
                (if (>= i (length ls))
                    #f
                    (if (compare-f elem (list-ref ls i))
                        i
                        (f (+ i 1) f))))))
        (loop-f 0 loop-f)))

;; (permanently) inserts the value into the list at the 
;; given index
;; 
;; ls            - list to update
;; val           - value to insert
;; ind           - index where the value will be inserted
(define (insert-at! ls val ind)
    (let ((len (length ls))
          (list-set! (lambda (ls ind val) (set-car! (list-tail ls ind) val)))  ;; sets the value at given index
          (snoc! (lambda (ls val) (set-cdr! (list-tail ls (- (length ls) 1)) (cons val '()))))) ;; appends val to end of list
        (if (and (>= ind 0) (< ind len))
            (let ((push-down!
                    (lambda (oi ni f)
                        (if (>= oi ind)
                            (let ((o-val (list-ref ls oi)))
                                (begin
                                    (if (< ni len)
                                        (list-set! ls ni o-val)
                                        (snoc! ls o-val))
                                    (f (- oi 1) (- ni 1) f)))))))
                (begin 
                    (push-down! (- len 1) len push-down!)
                    (list-set! ls ind val)))
            (snoc! ls val))))


;; removes element at the given index from the list
;;
;; ls            - list to update
;; ind           - index
(define (remove-element-at! ls ind)
    (let ((len (length ls)))
        (if (< ind len)
            (let ((val (list-ref ls ind))
                  (push-up!
                    (lambda (oi ni f)
                        (if (= oi len)
                            (set-cdr! (list-tail ls (- ni 1)) '())
                            (begin
                                (set-car! (list-tail ls ni) (list-ref ls oi))
                                (f (+ oi 1) (+ ni 1) f))))))
                (push-up! (+ ind 1) ind push-up!)
                val)
            #f)))

(define (make-list-of list-length val)
    (if (= 0 list-length) '() (cons val (make-list-of (- list-length 1) val))))

(define (copy-list ls) (map (lambda (a) a) ls))
