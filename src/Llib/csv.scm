(module csv
   (export +csv-lexer+
	   +tsv-lexer+
	   +psv-lexer+
	   (read-csv-record in #!optional (lexer +csv-lexer+))
	   (read-csv-records in #!optional (lexer +csv-lexer+))
	   (csv-for-each proc in #!optional (lexer +csv-lexer+))
	   (csv-map proc in #!optional (lexer +csv-lexer+))))



(define-macro (make-csv-lexer sep quot)
   (if (and (char? sep)
	    (char? quot))
       `(let ((in-quote? #f))
	   (regular-grammar ((quote ,quot)
			     (separator ,sep))
	      ((when in-quote?
		  (: quote quote))
	       (cons '2quote (string ,quot)))
	      (quote
		 (begin
		    (set! in-quote? (not in-quote?))
		    (cons 'kwote (the-string))))
	      
	      (separator
		 'separator)
	      ((or (: #\return #\newline)
		   #\newline)
	       'newline)
	      ((when (not in-quote?)
		  (+ (out quote separator #\return #\newline)))
	       (cons 'text (the-string)))
	      ((when in-quote?
		  (+ (out quote)))
	       (cons 'text (the-string)))
	      (else 
	       (let ((c (the-failure)))
		  (if (eof-object? c)
		      c
		      (error 'csv-lexer "Illegal character" c))))))
       (error 'csv-lexer "separator and quote must be a single character" (list sep quot))))


(define +csv-lexer+ (make-csv-lexer #\, #\"))

(define +tsv-lexer+ (make-csv-lexer #\tab #\"))

(define +psv-lexer+ (make-csv-lexer #\| #\"))


(define +csv-parser+
   (lalr-grammar (kwote 2quote separator newline text)
      (fields
	 ((field)
	  (list field))
	 ((field separator fields)
	  (cons field fields)))

      (field
	 (()
	  "")
	 ((text)
	  text)
	 ((escaped)
	  escaped))
      
      (escaped
	 ((kwote edata kwote)
	  edata))

      (edata
	 ((edatum)
	  edatum)
	 ((edatum edata)
	  (string-append edatum edata)))

      (edatum
	 ((text)
	  text)
	 ((2quote)
	  2quote))))
		 
	 
   
;;; 
(define (read-csv-record in #!optional (lexer +csv-lexer+))
   (let ((pc (peek-char in)))
      (if (eof-object? pc)
	  pc
	  (read/lalrp +csv-parser+ lexer in
	     (lambda (x) (or (eof-object? x)
			     (eq? x 'newline)))))))

(define (read-csv-records in #!optional (lexer +csv-lexer+))
   (let loop ((curr (read-csv-record in lexer))
	      (res '()))
      (if (eof-object? curr)
	  (reverse! res)
	  (loop (read-csv-record in lexer)
	     (cons curr res)))))


(define (csv-for-each proc in #!optional (lexer +csv-lexer+))
   (let loop ((curr (read-csv-record in lexer)))
      (if (eof-object? curr)
	  #unspecified
	  (begin
	     (proc curr)
	     (loop (read-csv-record in lexer))))))


(define (csv-map proc in #!optional (lexer +csv-lexer+))
   (let loop ((curr (read-csv-record in lexer))
	      (res '()))
      (if (eof-object? curr)
	  (reverse! res)
	  (loop (read-csv-record in lexer)
	     (cons (proc curr) res)))))