;;;;---------------------------------------------------------------------
;;
;;  Copyright © 2011
;;
;;  The purpose of this program is to index all occurrences of certain
;;  words inside a corpus. Those words are specified by the user via
;;  the argv. a corpus is a collection of plaintext representative of
;;  a given language. This program was written for my wife as an aid
;;  for her scientific researches (she is a Linguist and teaches at
;;  Columbia University).
;;  The resulting output of this program (a collection of
;;  files named after the words the user specified for indexing, each
;;  containing a set of forms (s-expressions) whose car is the
;;  filename of each file in which there are occurrences, and whose
;;  cdr is the list of said occurrences) is suited for searching in
;;  several ways, although right now all we are doing with it is
;;  searching for words before and after the indexed words. New ways
;;  to search will be added when my wife needs them.
;;  The invocation is:
;;  $ ./indexer corpus/folder words to be indexed
;;;;---------------------------------------------------------------------


(use posix)              ; create-directory, find-files, etc
(use srfi-69)            ; hash tables
(use srfi-13)            ; string functions (xsubstring, etc)
(use srfi-1)             ; list functions
(require-library utf8)   ; we need utf8 to save files with utf8 names
(use (prefix utf8 utf8-))
(require-extension utf8-case-map)   ; makes string-downcase work for utf8

;;;;
;; chicken has string-map, string-for-each, and string-for-each-index
;; but it lacks a HOF that provides both the char and the position.
;; this is the missing HOF.
(define (string-for-each-char-index proc str)
  (string-for-each-index (lambda (i) (proc (string-ref str i) i)) str))

;;;;
;; indeed, /home/john == /home//john, but it is more convenient for
;; other functions that we do not allow /home//john to happen
(define (ensure-path-ends-in-slash path)
  (if (string-suffix? "/" path) path (string-append path "/")))

;;;;
;; given "/home/john/file.txt", returns "file.txt"
(define (get-base-name filepath)
  (substring filepath (add1 (string-index-right filepath #\/))))

;; ./indexer <PATH> words to be indexed
(define corpus-folder (ensure-path-ends-in-slash (second (argv))))

;; ./indexer path <WORDS TO BE INDEXED>
(define wanted-words
  (map list->string
       (map utf8-string->list
            (cddr (argv)))))


;;;;
;; the chars that make up a word. any other char not contained in valid-chars
;; is considered a word-border ( same idea as "\b" in Perl regexps,
;; but fine-tunable to one's needs )
(define valid-chars
  ;; this is just so it looks nice within 80 columns :)
  (apply append (map utf8-string->list
   (list
    "abcdefghijklmnopqrstuvwxyz-ABCDEFGHIJKLMNOPQRSTUVWXYZ" ; ascii
    "áàãâéêíóõôúüçÁÀÃÂÉÊÍÓÕÔÚÜÇ"))))                        ; pt-br

;;;;
;; process-file kick-starts the chain of function calls
;; it slurps the file to memory, then calls process-string
(define (process-file hash filename)
  (let* ((file-contents (car  ; file-read's result is a list of one item
                         ;; open the file for reading, read it whole
                         ;; into the content variable and return that
                         (let* ((f (file-open filename open/read))
                                (content (file-read f (file-size f))))
                           (file-close f)
                           content))))
    (process-string hash file-contents)))

;;;;
;; process-string maps process-char over the chars.
;; the reason why process-file and process-string are separate
;; functions is because we anticipate the need to use
;; process-string without process-file in the future.
(define (process-string hash str)
  (string-for-each-char-index
   (lambda (c i) (process-char hash c i)) str))

;;;;
;; this is to make the following (which appears in process-char) more readable:
;; (set! current-word (string-append current-word (string c)))
(define-syntax string+=!
  (syntax-rules ()
    ((_ lhs rhs)
     (set! lhs (string-append lhs rhs)))))

;;;;
;; @@ this function maintains internal state @@
;; process-char is called from process-word.
;; it skips over non valid-chars and accumulates contiguous
;; valid-chars into current-word; when a contiguous sequence of
;; valid-char ends, it calls process-word with the word it accumulated
;; and with current-index, i.e. the position of the beginning of the
;; word in the file.
(define process-char
  (let ((last-was-separator #t)  ; was the last char a word border?
        (current-word "")        ; current word being built
        (current-index -1))      ; position in file where curr word starts    
    (lambda (hash c pos)
      ;; if this char is a valid word char
      (cond ((member c valid-chars)
             ;; add char c to current word regardless of whether last char
             ;; was a separator
             (string+=! current-word (string c))
             ;; if last char was a separator, this char begins a word
             (cond (last-was-separator
                    (set! current-index pos)
                    (set! last-was-separator #f))))
            ;; if this isn't a valid word char, then it's a word-border
            ;; we now have a word and its position in the file, so
            ;; we're ready to process the word
            (else (if (not last-was-separator)
                      (process-word hash current-word current-index))
                  ;; ^ only process the word if we just finished
                  ;; building one. otherwise all sorts of punctuations
                  ;; and numbers would be processed. that happens
                  ;; because this 'else' is called every time c is not
                  ;; a valid char, which also happens outside of
                  ;; building a word.
                  (set! last-was-separator #t)
                  (set! current-word ""))))))

;;;;
;; process-word is called from process-char. it takes a word and a
;; position and adds these to the index only if it's a wanted-word.
(define process-word
  (lambda (hash word pos)
    ;; add the word to the index if it's one of the wanted ones
    (if (member word wanted-words)
        (add-to-index hash word pos))))

;;;;
;; add-to-index is called from process-word.
;; finds <word> key in <hash> and conses <pos> to its value
;; if the <word> key isn't there, it creates it with value '() and
;; conses to that. This hash is case-insensitive; all words are saved
;; as lowercase.
(define add-to-index
  (lambda (hash word pos)
    (hash-table-set!
      hash
      (string-downcase word)
      ; before we cons:       after we cons:
      ; ("word" pos1 pos2)    ("word" newpos pos1 pos2)
      (cons pos (hash-table-ref/default hash word '())))))

;;;;
;; stringify-hash has to be called after process-file; that means the
;; hash has been filled with information on the wanted words and their
;; indices. stringify-hash takes advantage of hash-table-walk which is
;; a built-in function that acts like map but for each key-value pair.
;; it gives fappend-word to the walking function in order to save files.
(define (stringify-hash hash filename)
  (begin
    (hash-table-walk hash
                     (lambda (w i) (fappend-word w i filename)))))

;;;;
;; this is used to append a new line like this:  ("<corpus-file>" <indices>)
;; to <word>.txt where <indices> is e.g. 12 78 230...
(define fappend-word
  (lambda (word indices corpus-file)
     ;; we need to convert the word to utf8 because the OS filenames
     ;; use that. the easiest way is like so
     (let* ((filename (utf8-case-map#utf8-string-downcase
                       (utf8-list->string (string->list word))))
            (index-file (string-append
                         corpus-folder "index/" filename ".txt")))
       (with-output-to-file index-file
         ;; write allows me to print the form ("<corpus-file"
         ;; <indices>) to the file as if it were code
         (lambda () (write (cons corpus-file indices)) (newline)) #:append))))

;;;;
;; this is a simple closure/object over some functions/methods.
;; it abstracts the functionality/data shared by those functions.
;; these are used to get and set information about the size of the
;; corpus and the percent of the corpus that has been processed so far.
(let ((partial-size 0) (total-size 0))
  (set! set-total-corpus-size
        (lambda (s)
          (set! total-size s)))
  (set! get-total-corpus-size
        (lambda ()
          total-size))
  (set! add-partial-corpus-size
        (lambda (s)
          (set! partial-size (+ partial-size s))))
  (set! get-partial-corpus-size
        (lambda ()
          partial-size))
  (set! add-file-to-partial-corpus-size
        (lambda (f)
          (set! partial-size (+ partial-size (file-size f)))))
  (set! get-percent-done
        (lambda ()  ; partial-size / total-size * 100
          (/ partial-size total-size 0.01))))

;;;;
;; self-explanatory.
(define (file-hidden? f)
  (string-prefix? "." (get-base-name f)))

;;;;
;; find-files takes a path and a predicate. the predicate here is that
;; it is not a directory nor a hidden file (dot-file)
(define (list-files-from-dir dir)
  (find-files
   dir
   (lambda (f) (and (regular-file? f) (not (file-hidden? f))))))

;;;;
;; this is (hopefully) the equivalent of $ du -sk folder/ but in bytes
;; (as if there were a du -sb)
(define (get-folder-size folder)
  (fold (lambda (x y) (+ (file-size x) y))
        0
        (list-files-from-dir folder)))

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
;; @@ this function maintains internal state
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

;;;(define string-length-corpus-folder (string-length corpus-folder))

(define (main)
  ;; creates an index/ folder inside the corpus-folder
  (create-directory (string-append corpus-folder "index"))
  ;; this should be self-explanatory since it is so readable :)
  (set-total-corpus-size (get-folder-size corpus-folder))
  ;; draw the size of the progresss bar (ncurses would be overkill here)
  (init-progress-bar)
  (map
   (lambda (filename)
     ;; make a new hash for each file; pre-allocate 10k k,v-pairs
     (let* ((index-hash (make-hash-table #:size 10000))
            (strlen-corpus-folder (string-length corpus-folder))
            ;; cur-file is what comes after corpus-folder/ (filename is
            ;; a full absolute path so we have to cut it).
            ;; we use cur-file as a string to be saved to files,
            ;; because we want a path that is relative to corpus-folder
            (current-file (substring filename strlen-corpus-folder)))
       
       ;; process-file takes the full path because it needs to open the file
       (process-file index-hash filename)
       ;; this takes the current-file because it will save it as a string
       (stringify-hash index-hash current-file)
       
       ;; this part deals with user feedback
       (add-file-to-partial-corpus-size filename)
       (print-progress-bar (get-partial-corpus-size)
                           (get-total-corpus-size))))
   ;; ^ this whole lambda above is mapped onto the list of files from the
   ;; corpus folder.
   (list-files-from-dir corpus-folder)))

(main)
(newline)
(display "done.")
(newline)

