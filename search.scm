;;;;---------------------------------------------------------------------
;;
;;  Copyright © 2011
;;
;;  The purpose of this program is to rank by frequency the words that
;;  come immediately before or after a given word. This given word is
;;  supplied via argv. The invocation is thus:
;;  $ ./search corpus/folder word after
;;  $ ./search corpus/folder word before
;;  depending on whether one wishes for the words before or after the
;;  given word to be ranked.
;;
;; Here's how it works (the given word in this example will be "not")
;; 1 - get-afterword-from-word opens "not.txt"
;; 2 - it maps get-afterword-from-form over the contents of "not.txt"
;; 3 - that function  maps string-tokenize over get-bytes-after-word
;; 4 - which is an abstraction of read-indices
;; 5 - which does the grunt work of opening the file in the car of
;; index-form and retrieving a certain amount of bytes after each
;; occurrence of "not" given by the cdr of index-form
;; 6 - after all of that is done, we have a list of lists of strings,
;; each with a single word that was found immediately after each
;; occurrence of "not".
;; 7 - we now flatten that list, then sort it alphabetically, then
;; group it by equality, then sort it by length, then map car and
;; length over it and then print that. *phew*
;; 8 - the same is true mutatis mutandis of get-beforeword-from-word
;;
;;;;---------------------------------------------------------------------

(use posix)              ; file operations
(use srfi-1)             ; filter, last, list functions
(use srfi-8)             ; receive
(use srfi-13 srfi-14)    ; string functions
(require-library utf8)   ; needed for printing in utf8
(use (prefix utf8 utf8-))


;;;;
;; indeed, /home/john == /home//john, but it is more convenient for
;; other functions that we do not allow /home//john to happen
(define (ensure-path-ends-in-slash path)
  (if (string-suffix? "/" path) path (string-append path "/")))

;; the other argvs are bound inside (main)
;; this is global because it is shared by many functions
;; and it wouldbe cumbersome to pass this around
(define corpus-folder (ensure-path-ends-in-slash (second (argv))))

;; this is how many bytes we want to fetch before or after a word
;; (depending on the get-*word- function)
;; this is not exposed to the user via argv because it's too technical
(define surrounding-bytes 40)

;;;;
;; string-tokenize takes char-sets. that's a faster way to do the PCRE
;; equivalent of [a-zA-Z...] without having to ask if a char is a
;; member of a char-list. So we build a char-set of valid word chars
(define valid-chars-char-set
  (string->char-set
   (list->string (utf8-string->list
   "áàãâéêíóõôúüçÁÀÃÂÉÊÍÓÕÔÚÜÇabcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))

;;;;
;; this is an import I wrote of Haskell's groupBy function.
;; it groups contiguous items that satisfy the predicate "eq" in a
;; list. the only predicate I'm concerned with is equal?.
;; N.B. the list must be sorted for equal items to be contiguous ;)
(define (group-by eq lst)
  (if (null? lst)
      '() 
      (let*-values (((x) (car lst))
                    ((xs) (cdr lst))
                    ((ys zs) (span (lambda (i) (eq i x)) xs)))
        (cons (cons x ys) (group-by eq zs)))))
(define (group lst) (group-by equal? lst))
;;; Haskell's groupBy function:
;;; groupBy _  []       =  []
;;; groupBy eq (x:xs)   =  (x:ys) : groupBy eq zs
;;;     where (ys,zs) = span (eq x) xs
;;; group = groupBy (==)


;;;;
;; read-indices is called from get-bytes-after-word;
;; its job is to map an <index-form> (see below) onto a list of
;; strings of length <bytes> which are the strings that come after
;; each index+offset. so if blah.txt had "hi mom hi dad" in it, then
;; hi.txt would have a form ("blah.txt" 0 7) and calling read-indices
;; on that form, 4 bytes and 2 offset would yield (" mom" " dad")
;; try it yourself: (read-indices '("../blah.txt" 0 7) 4 2)
;; index-form  : ("file.txt" 20 89 176)
;; bytes       : bytes to read from each index+offset onward
;; offset      : usually the word length (so we start reading bytes
;;               right after the end of the word that is indexed, but
;;               we use a negative number for -before-word.
;; returns a list of 'bytes'-long strings
(define (read-indices index-form bytes offset)
  (call-with-input-file
      ;; 
      (string-append corpus-folder (car index-form))
    (lambda (f)       ; f is the file descriptor
      (map       ; we map, over the indices of index-form (cdr index-form),
       (lambda (i)    ; a lambda that sets position on file and reads bytes
         (set-file-position! f (+ offset i))
         (string-downcase (read-string bytes f)))
       (cdr index-form)))))

;;;;
;; this is just one wrapper over read-indices that takes care of the
;; word-length and how many bytes to read for us.
;; e.g. if the word is "not" and in a file there's the string
;; "I am not a warrior(...)", then this will only read " a
;; warrior(...)", i.e. it will avoid reading "not"
(define (get-bytes-after-word index-form word)
  (let ((word-len (string-length word)))
    (read-indices index-form surrounding-bytes word-len)))

;; the equivalent 'before' functions can be easily derived from their
;; 'after' counterparts' implementation. they should be clear by contrast.
(define (get-bytes-before-word index-form word)
  (read-indices index-form surrounding-bytes (- surrounding-bytes)))

;;;;
;; Haskell teaches us never to use "head" (car) because it doesn't
;; handle empty lists. In haskell it's best to wrap head in a Maybe
;; monad, but in Scheme for our purposes it's more idiomatic to write
;; a car-with-default
(define (car/default lst default)
  (if (pair? lst)
      (car lst)
      default))

(define (last/default lst default)
  (if (pair? lst)
      (last lst)
      default))

;;;;
;; to ask if a word exists (i.e. has been indexed by the indexer),
;; this is the function to use. here we're only concerned with the
;; boolean value of what file-exists? returns (see find-file-from-word)
(define (word-exists? word)
  (file-exists?
   (string-append corpus-folder "index/"  word ".txt")))

;;;;
;; this is an alias to word-exists? because they express different
;; intents. the intent of this alias is to get the full path of
;; <word>.txt (file-exists? returns either #f or the full path)
(define find-file-from-word word-exists?)

;;;;
;; we invent the term "afterword" to mean the word that comes after a word.
;; since get-bytes-after-word returns a list of strings, in order to
;; get the first word out of each string all we have to do is map
;; string-tokenize over that list (and get the first/last element)
(define (get-afterword-from-form index-form word)
  (map (lambda (each) (car/default (string-tokenize each valid-chars-char-set) ""))
       (get-bytes-after-word index-form word)))

(define (get-beforeword-from-form index-form word)
  (map (lambda (each) (last/default (string-tokenize each valid-chars-char-set) ""))
       (get-bytes-before-word index-form word)))

;;;;
;; this function is a bit more powerful than the preceding ones, since
;; this is where the actual reading of <word>.txt takes place. we map
;; get-afterword-from-form over the contents of <word>.txt.
;; NB this function returns a list of lists of string (each sublist
;; represents one file)
;; NB since there's a lot of common code between these two functions,
;; I've decided to put the boilerplate code inside
;; build-word-getter-fn and define the actual functions in terms of
;; that.
;; these 2 functions take a word and a feedback function that will be
;; called each time a form is processed. that feedback function  will
;; be fed the number of forms processed so far and the total number of
;; forms.
(define (build-word-getter-fn get-*word-fn)
  (lambda (word feedback-fn)
    ;; 'xss' just because it's a list of lists
    (let* ((xss (read-file (find-file-from-word word)))
           (total-forms (length xss))
           (forms-done-so-far 0))
      ;; ^ total-forms and forms-done-so-far are passed to the
      ;; feedback function and aren't actually useful for what this
      ;; function does.
      (map
       (lambda (each)
         (set! forms-done-so-far (add1 forms-done-so-far))
         (feedback-fn forms-done-so-far total-forms)
         (get-*word-fn each word))  ; this is the only line that isn't shared
       (read-file (find-file-from-word word))))))

(define get-afterword-from-word
  (build-word-getter-fn get-afterword-from-form))
(define get-beforeword-from-word
  (build-word-getter-fn get-beforeword-from-form))

;;;;
;; self-explanatory, but see get-afterword-from-word for the reason
;; why we need to flatten its list.
(define (flatten-and-sort-alphabetically lst)
  (sort (flatten lst) string-ci<))

;;;;
;; '((1 2) (1) (1 2 3)) => ((1 2 3) (1 2) (1))
(define (sort-by-length-desc lst)
  (sort lst (lambda (a b) (> (length a) (length b)))))
;;;;
;; '((1 2) (1) (1 2 3)) => ((1) (1 2) (1 2 3))
(define (sort-by-length-asc lst)
  (sort lst (lambda (a b) (< (length a) (length b)))))

;;;;
;; (rank-by-frequency
;;   sort-by-length-asc '(("a" "an") ("the" "an") ("an" "the" "an"))) =>
;;    (("a") ("the" "the") ("an" "an" "an" "an"))
;; note that if we then map (length x, car x) over the resulting
;; list, we'd have the tuples: [(1, "a"), (2, "the"), (4, "an")],
;; which is a very useful list to have, to say the least :)
(define (rank-by-frequency sort-fn lst)
  (sort-fn (group (flatten-and-sort-alphabetically lst))))

;;;;
;; print-rank maps car and length over the result of rank-by-frequency
;; and prints it, for a given word. it assumes the word exists.
;; word       : the word for which to print the rank
;; sort-fn    : how to sort the results
;; getword-fn : which get-*word- function
(define (print-rank word sort-fn getword-fn feedback-fn)
  ;; terminals and filesystems nowadays support unicode, so we need an
  ;; ad hoc function to encode a string in utf8
  (let ((utf8ize (lambda (str) (utf8-list->string (string->list str))))
        ;; rank will be a list we can map car and length over
        (rank (rank-by-frequency sort-fn (getword-fn word feedback-fn))))
    (for-each
     (lambda (i) (printf "~a : ~a~n" (length i) (utf8ize (car i))))
     rank)))

;;;;
;; this takes advantage of xsubstring
;; string-repeat "w3" 3 => "w3w3w3"
(define (string-repeat str n)
  (xsubstring str 0 (* n (string-length str))))

;;;;
;; prints | followed by 98 _ followed by |
(define (init-progress-bar)
  (display (string-append "|" (string-repeat "_" 98) "|"))
  (newline)
  (flush-output))

;;;;
;; @@ this function maintains internal state @@
;; this function should be rather straightforward
;; feed it what you have done so far and the total and it will print
;; however many more '=' chars are needed to make the printed
;; progress-bar represent the percentage of <so-far>
(define print-progress-bar
  (let ((last-percent 0))
    (lambda (so-far total)
      (let ((percent-done (floor (/ so-far total 0.01))))
        (display
         (string-repeat "="
                        (inexact->exact (- percent-done last-percent))))
      (flush-output)
      (set! last-percent percent-done)))))

;; ./search corpus/folder word direction
(define (main)
  (let ((word (third (argv)))
        (direction (fourth (argv))))
    (init-progress-bar)
    (if (word-exists? word)
        (print-rank word
                    sort-by-length-asc
                    ;; choose which get-*word function based on the
                    ;; direction. it's not too late to check for this
                    ;; error, but also we don't expect this to be a
                    ;; common error.                    
                    (case (string->symbol direction)
                      ((after)  get-afterword-from-word )
                      ((before) get-beforeword-from-word)
                      (else (error "Direction must be 'before' or 'after'")))
                    print-progress-bar)
        (display (format "~nthe word ~a has not been indexed.~n" word)))))

(main)


