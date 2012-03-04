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


(define-macro (make-csv-lexer sep quot)
   (if (and (char? sep)
	    (char? quot))
       `(lambda (in-quote?)
	   (regular-grammar ((quote ,quot)
			     (separator ,sep))
	      ((when in-quote?
		  (: quote quote))
	       (cons '2quote (string ,quot)))
	      (quote
		 (begin
		    (set! in-quote? (not in-quote?))
		    (cons 'kwote (the-string))))
	      ((when (not in-quote?)
		  (+ (or #\space #\tab)))
		  (cons 'space (the-string)))
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