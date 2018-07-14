;;;; Copyright(c) 2011-2014 Joseph Donaldson(donaldsonjw@yahoo.com) 
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

(define +csv-unspecified+ '(#unspecified))

(define (read-csv-record in #!optional (lexer +csv-lexer+))
   (when (not (input-port? in))
      (raise (instantiate::&io-port-error (proc "read-csv-record")
                                          (msg "invalid input port")
                                          (obj in))))
   (let loop ((token (read/rp lexer in))
              (last-token +csv-unspecified+)
              (res '()))
      (cond ((or (eq? token 'newline)
                 (eof-object? token))
             (if (and (eof-object? token)
                      (eq? last-token +csv-unspecified+))
                 #eof-object
                  (reverse! res)))
            ((and (pair? token)
                  (eq? (car token) 'text))
             (loop (read/rp lexer in)
                (car token)
                (if (eq? last-token 'text)
                    (cons (string-append (car res) (cdr token)) (cdr res))
                    (cons (cdr token) res))))
            ((eq? token 'separator)
             (loop (read/rp lexer in)
                'separator
                (if (eq? last-token 'separator)
                    (cons "" res)
                    res)))
            (else
             (loop (read/rp lexer in)
                'text
                res)))))

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