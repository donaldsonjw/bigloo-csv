;;;; Copyright(c) 2011, 2012 Joseph Donaldson(donaldsonjw@yahoo.com) 
;;;; This file is part of bigloo-csv.
;;;;
;;;;     bigloo-csv is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU Lesser General Public License as
;;;;     published by the Free Software Foundation, either version 3 of the
;;;;     License, or (at your option) any later version.
;;;;
;;;;     bigloo-csv is distributed in the hope that it will be useful, but
;;;;     WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;     Lesser General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU Lesser General Public
;;;;     License along with bigloo-csv.  If not, see
;;;;     <http://www.gnu.org/licenses/>.
(module csv
   (include "csv.sch")
   (export +csv-lexer+
	   +tsv-lexer+
	   +psv-lexer+
	   (read-csv-record in #!optional (lexer +csv-lexer+))
	   (read-csv-records in #!optional (lexer +csv-lexer+))
	   (csv-for-each proc in #!optional (lexer +csv-lexer+))
	   (csv-map proc in #!optional (lexer +csv-lexer+))))



(define +csv-lexer+ (make-csv-lexer #\, #\"))

(define +tsv-lexer+ (make-csv-lexer #\tab #\"))

(define +psv-lexer+ (make-csv-lexer #\| #\"))


(define +csv-parser+
   (lalr-grammar (kwote 2quote space separator newline text)
      (fields
	 ((field)
	  (list field))
	 ((field separator fields)
	  (cons field fields)))
      
      (field
	 (()
	  "")
	 ((possible-space@a text possible-space@b)
	  (string-append a text b))
	 ((possible-space@a escaped possible-space@b)
	  escaped))
	 
      (possible-space
	 (()
	  "")
	 ((space)
	    space))
      
      (escaped
	 ((kwote kwote)
	  "")
	 ((kwote edata kwote)
	  edata))
      
      ; (escaped
      ; 	 ((possible-space+kwote kwote+possible-space)
      ; 	  "")
      ; 	 ((possible-space+kwote edata kwote+possible-space)
      ; 	  edata))

      ; (possible-space+kwote
      ; 	 ((kwote)
      ; 	  kwote)
      ; 	 ((space kwote)
      ; 	  kwote))
      ; (kwote+possible-space
      ; 	 ((kwote)
      ; 	  kwote)
      ; 	 ((kwote space)
      ; 	  kwote))
	    
   
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
   (if (input-port? in)
       (let ((pc (peek-char in)))
	  (if (eof-object? pc)
	      pc
	      (read/lalrp +csv-parser+ (lexer #f) in
		 (lambda (x) (or (eof-object? x)
				 (eq? x 'newline))))))
       (raise (instantiate::&io-port-error (proc "read-csv-record")
					   (msg "invalid input port")
					   (obj in)))))

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