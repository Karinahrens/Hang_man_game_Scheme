;; hang-man for REPL Scheme

;;
(define source-name "glossary.txt")

;; Side effect:
;; Strig-> IO([String])
;; Passed the path, open the file containig glossary
(define (read-words-from filename)
  (let* ((port (open-input-file filename))
         (res (read-word-list port '())))
    (close-input-port port)
    res))

;; Side effect
;; Fd -> [String] -> IO ([String])
;; Passed port and acumulator, return the all the words as strings
(define (read-word-list port acc)
  (let ((stuff (read port)))
    (if (eof-object? stuff)
        acc
        (read-word-list port
                        (cons (symbol->string stuff) acc)))))

(define list-of-words (read-words-from source-name))

;; STATE OF THE GAME
(define word-to-guess null)
(define partial-sol null)

(define hits 0)
(define plays 0)
(define failures 0)
(define total-failures 6)
(define total-hits (length word-to-guess))
(define glossary (map string->list list-of-words))


;; 
;; IO(String)
(define (game-status)
  (begin
    (format "~a H:~a/~a F:~a/~a ~a ~a"
            (list->string partial-sol)
            hits  total-hits
            failures  total-failures
            plays
            (if (and
                 (< hits total-hits)
                 ;(< failures total-failures)
                 )
                ""
                (string-append "GAME-OVER(" (list->string word-to-guess) ")")))))

          

;;;
;;  PURELY FUNCTIONAL
;;
; Given a (possibly empty) list of chars and a char, it counts how many
;times the given char occurs in the list.
(define (occurrences lst x)  
  (cond
    [(empty? lst)0]  ; list possibly empty of chars
    [(equal? (car lst) x) (+ 1 (occurrences (cdr lst) x))] ; if list equal to char, counts
    [else (occurrences (cdr lst) x)])) ;applying to the rest of the list


;testing
(occurrences '(#\a #\b #\b) #\b)
(occurrences '() #\b)
;(occurrences '(1 2 3) 1)

;Given a (possibly empty) list of chars and a char, it returns a list of indices
;for the char’s occurrences in the given list
; Source code: https://stackoverflow.com/questions/42559067/scheme-finding-all-indexes-of-occurrences-
;of-a-list-element

(define (indices lst element)
  (let ile ((i 0)            ;tail recursion
            (a '())          ;list result
            (l lst))         ;remaining input list
    (cond
      [(empty? l)            
       (reverse a)]
      [(eq?  (car l) element)            ;if list and element qual
       (ile (+ i 1) (cons i a) (cdr l))] ;increase index and add to the list result
      [else                              ; otherwise
       (ile (+ i 1) a (cdr l))]          ; increase index but dont add to the list result
      )))


; testing
(indices  '() #\b)
(indices '(#\a #\b #\b) #\b)
;(indices '() 1)
;(indices '(1 2 1 3) 1)
;(indices '( 1 1 1 1) 2)

;Given a (possible empty) list of chars, a list of integers
;representing indices where values n ∈ [0. . L), L the length of the list, and a char, yield a new list obtained
;from the original where the character at each of the given indices is replaced by the given char.
;source code: https://webcache.googleusercontent.com/search?q=cache:
;eDa7Y_ichqMJ:https://stackoverflow.com/questions/66583441/how-do-i-solve-the-following-question-on-drracket-using-scheme+&cd=1&hl=en&ct=clnk&gl=uk

(define (replace-indices lst indices replacement)
  (if (or (null? lst) (null? indices)) ; if list or index empty return list
      lst
      (let lir ((lt lst)               ; recursive
                (cdl (cdr lst))        ; represents rest of list
                (cal (car lst))        ; represents first item in list
                (lsa '())              ; reversed list to fix "wrong order"
                (ind indices)          ; indices 
                (ina '()))             ; list that will decrease
        (cond
          ((null? ind)                 ; when indices empty
           (if (null? cdl)             ; when list empty
               (reverse (cons cal lsa)); reverse and add to the acumulator
               (lir cdl (cdr cdl) (car cdl) (cons cal lsa) ; calling lir to add to the decreased list
                    ina '())))
          ((zero? (first ind)) ; when 0 take indices and apply loop
           (lir lt cdl replacement lsa
                (cdr ind) ina))
          (else                ; repeat with the rest itens of the list 
           (lir lt cdl cal lsa
                (cdr ind) (cons (- (car ind) 1) ina)))))))

; testing
(replace-indices '(#\a #\* #\*) '(1 2)  #\b)
(replace-indices '(#\a #\* #\*) '() #\b)
;(replace-indices '(1 2 3) '(1) #\b)
;(replace-indices '(1 2 3) '(1) 3)

; Given a list of chars, it returns the number of chars different to “*”.
(define noOfHits
  (λ (lst)              
    (cond
      [(null? lst) 0]        ; if list empty, 0 
      [(not (eq? (car lst) '#\*)) (add1 (noOfHits (cdr lst)))] ; if not equal to * add1
      [else (noOfHits (cdr lst))]))) ;applying to the rest of the list

;testing
(noOfHits '(#\a #\* #\*))
(noOfHits '(#\* #\* #\*))
;(noOfHits '(1 2 3))
;(noOfHits '())


;; Side effects
;; IO(String)

(define (restart)
  (begin
    (set! word-to-guess (list-ref glossary (random (length glossary)))) ; Chooses a random word out of glossary
    (set! partial-sol (string->list(make-string (string-length(list->string word-to-guess)) #\*))) ; Convert the word to guess into a string to be able to display it in "*" and converts it back to a list.
    (set! plays 0) ;Set plays to 0
    (set! failures 0) ; Set plays to 0
    (set! total-hits (length word-to-guess)) ; Set total-hits to a number based on the length of the word-to-guess
    (set! total-failures 6) ; Set total-failures to 6
    (game-status)))


;(restart)

;e. Increase failures if the set of occurrences of given char is 0.
;(define indices-hit void)
;; Char -> IO(String)
(define (guess char)
  (define indices-hit void) ;Function that will define which hits have been hit
  (set! indices-hit (indices word-to-guess char)) ;Set this new function with the same variable as indices
  (set! partial-sol (replace-indices partial-sol (indices word-to-guess char) char)) ;Display any chars that have hit into the correct indices of partial-sol
  (set! hits (noOfHits partial-sol)) ;Counts the hits based on partial-sol
  (set! plays (+ 1 plays)) ;Increase the number of plays every time the user guesses a character
  (set! failures (cond ((=(occurrences word-to-guess char)0)(+ 1 failures))(else ( + 0 failures) ))) ;Creates a condition that will increase the number of failures by 1 when occurrences = 0.
  (game-status))
    
  
;(restart)
;(guess #\z)
;(guess #\a)
;(guess #\b)
;(guess #\c)
;(guess #\d)


;iii. a loop (for-each), guessing every character of the given string. After that, it invokes statement game-status.
;; IO(String)
(define (solve word)
  (begin
    (for-each guess (string->list word)) ;Begins a for-each loop that will guess every char of the string given and increase failures based on it.                                              
    (game-status)))

;(restart)
;(solve "calamites")
;;
;; EXTRA -F3
;;;;;;;;;;;;;;;;;;;;;;;;;;
   
;; p: all-words as list of list of char
;i. Write a function predicate called words-containing. Given a (possibly empty)
;list of words (as a list of lists of chars) and a char which yields only the
; words containing that char
; source-code: https://stackoverflow.com/questions/66726521/filtering-words-from-a-list-that-matches-my-char-in-scheme
(define (words-containing all-words char)
  (filter (λ (all-words)(member char all-words))all-words))
     
;(restart)
;(words-containing '((#\b #\u #\s)
;                    (#\b #\a #\r)
;                    (#\c #\a #\r))
;                  #\a)

;(words-containing '((#\b #\u #\s)
;                    (#\b #\a #\r)
;                    (#\c #\a #\r))
;                  #\b)
;(words-containing '((#\b #\u #\s)
;                    (#\b #\a #\r)
;                    (#\c #\a #\r))
;                  #\z)
;(words-containing '((#\b #\u #\s)
;                    (#\b #\a #\r)
;                    (#\c #\a #\r))
;                  #\2)
;(words-containing '((#\b #\u #\s)
;                    (#\b #\a #\r)
;                    (#\c #\a #\r))
;                  #\c)
;; p: all-words as list of list of char
;;  : chars as a list of char
;(define (words-containing-ext all-words chars)
; ())
;(define (words-containing-ext all-words chars ))

;selects those members of l which satisfy p
;;;;
;(define (select_set p l)
;  (cond
;    ((null? l) '())
;    ((p (car l)) (cons (car l)(select_set p (cdr l))))
;    (else (select_set p (cdr l)))
;    )
;  )
;(select_set (= #\b) '((#\b #\u #\s)
;                    (#\b #\a #\r)
;                    (#\c #\a #\r)))

;(define (words-containing-ext all-words char)
 ; (filter (λ (all-words)(member ((cdr char) all-words)))all-words) 
  ;(words-containing-ext (all-words (member ((car char) all-words))) all-words))

;(words-containing-ext '((#\b #\u #\s)
   ;                     (#\b #\a #\r)
    ;                    (#\c #\a #\r))
     ;                 #\c #\a)

;; IO([String])
;; this is very hard.
(define (sieve chars) (void))

