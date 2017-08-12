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


(define-macro (make-csv-lexer sep quot)
   (if (and (char? sep)
	    (char? quot))
       `(regular-grammar ((quote ,quot)
			  (separator ,sep))
	   (quote
	    (let loop ((curr (read-char (the-port)))
		       (res '()))
	       (cond ((eof-object? curr)
		      (raise (instantiate::&io-parse-error (proc "lexer")
							   (msg "failed to parse fail")
							   (obj curr))))
		     ((and (char=? curr ,quot)
			   (not (eof-object? (peek-char (the-port))))
			   (char=? (peek-char (the-port)) ,quot))
		      (read-char (the-port))
		      (loop (read-char (the-port))
			 (cons ,quot res)))
		     ((char=? curr ,quot)
		      (cons 'text  (list->string (reverse! res))))
		     (else
		      (loop (read-char (the-port))
			    (cons curr res))))))
	   (separator
	    'separator)
	   ((or (: #\return #\newline)
		#\newline)
	    'newline)
	   ((+ (out quote separator #\return #\newline))
	    (cons 'text (the-string)))
	   (else 
	    (let ((c (the-failure)))
	       (if (eof-object? c)
		   c
		   (error 'csv-lexer "Illegal character" c)))))
   (error 'csv-lexer "separator and quote must be a single character" (list sep quot))))