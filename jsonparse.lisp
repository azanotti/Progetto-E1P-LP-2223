;;;; Federico Combi 886034
;;;; Alessandro Zanotti 885892

;;;; -*- Mode: Lisp -*-
;;;; jsonparse.l


;; in jsonparse 
;; take the string, transform in charlist,
;; sanitize it and give to jsonparseclist
(defun jsonparse (JSONString)
  	(if (not (stringp JSONString)) 
		(error "No string")
		(jsonparseclist (sanitizecharlist(stringlist JSONString)))
	)
)


;; in jsonparseclist take charlist sanitezed and parse it 
(defun jsonparseclist (charlist)
  	(cond 
		((and (eq (car charlist) #\{) (eq (car (last charlist)) #\}))
         	(cons 'jsonobj (parsemembers (removebraces charlist) nil 0 0))
		) 
        ((and (eq (car charlist) #\[) (eq (car (last charlist)) #\])) 
         	(cons 'jsonarray (parsearray (removebraces charlist) nil 0 0))
		)
        (T 
			(error "Syntax error: braces")
		)
	)
)
  

;; if ',' in sub obj not split
;; add counter for {}
;; openCounter = open parenthesis counter
;; closeCounter = closed parenthesis counter
(defun parsemembers (token container openCounter closeCounter)
  	(cond 
		((and (null token) (null container)) nil)
        ((null token) (list (parsepair container)))
		((eq (car (last token)) #\,) (error "Argument not found"))
        ((and (eq (car token) #\,) (= openCounter closeCounter))
	 		(cons 
				(parsepair container)
	       		(parsemembers (cdr token) nil openCounter closeCounter)
		   )
		)
        ((or (eq (car token) #\{) (eq (car token) #\[)) 
	 		(parsemembers 
				(cdr token) 
				(consend (car token) container)
				(incf openCounter) 
				closeCounter
			)
		)
        ((or (eq (car token) #\}) (eq (car token) #\])) 
	 		(parsemembers 
				(cdr token)
				(consend (car token) container)
				openCounter 
				(incf closeCounter)
			)
		)
        (T (parsemembers 
				(cdr token)
			  	(consend (car token) container)
			  	openCounter 
			  	closeCounter
			)
		)
	)
)


;; parsepair check attribute is a string
;; if value call parsevalue
(defun parsepair (token)
  	(cond 
		((null token) nil)
        ((not (stringp (car token))) (error "Attribute no string"))
		((not (eq (cadr token) #\:)) (error "No pair")) 
        (T (list (car token) (parsevalue (cdr (cdr token)))))
	)
)


;; value can be numbers, string, JSON obj
;; if obj call jsonparse
(defun parsevalue (value)
	(cond 
		((and (eq (first value) #\{) (eq (car (last value)) #\}))
         	(cons 
				'jsonobj 
          		(parsemembers (removebraces value) nil 0 0)
			)
		)
        ((and (eq (first value) #\[) (eq (car (last value)) #\]))
         	(cons 
				'jsonarray 
          		(parsearray (removebraces value) nil 0 0)
			)
		)
		((not (eq (cdr value) nil)) (error "No valid value"))
        ((stringp (car value)) (car value))
        ((numberp (car value)) (car value))
		(T (error "No valid value"))
	)
)


;; parsearray
(defun parsearray (token container openCounter closeCounter)
  	(cond 
		((eq (car (last token)) #\,) (error "Argument not found"))
		((and (null token) (null container)) nil)
        ((null token) (list (parsevalue container)))
        ((and (eq (car token) #\,) (= openCounter closeCounter))
	 		(cons 
				(parsevalue container)
	       		(parsearray (cdr token) nil openCounter closeCounter)
			)
		)
        ((or (eq (car token) #\{) (eq (car token) #\[)) 
	 		(parsearray 
				(cdr token)
		    	(consend (car token) container)
		      	(incf openCounter) 
				closeCounter
			)
		)
        ((or (eq (car token) #\}) (eq (car token) #\])) 
	 		(parsearray 
				(cdr token)
		      	(consend (car token) container)
		      	openCounter 
				(incf closeCounter)
			)
		)
        ((or (null container) (not (= openCounter closeCounter)))
         	(parsearray 
				(cdr token)
                (consend (car token) container)
                openCounter 
				closeCounter
			)
		)
        (T (error "Wrong array syntax"))
	)
)
	
;; transorm string to list
(defun stringlist (JSONString)
  	(if (= (length JSONString) 0) 
		nil
    	(cons 
			(char JSONString 0)
	  		(stringlist (subseq JSONString 1))
		)
	)
)

;; trasform list to string
(defun liststring (charlist)
  	(if (null charlist) 
		""
    	(concatenate 
			'string
		 	(string (car charlist))
		 	(liststring (cdr charlist))
		)
	)
)


;; sanitize char list
(defun sanitizecharlist (charlist) 
  	(remove #\Return 
	  	(remove #\Tab 
		  	(remove #\Newline 
			  	(remove #\Space 
				  	(reducenumbers 
				   		(reducestring 
				    		(substitute #\" #\' charlist)
				    		nil 0
						)
				   		nil
					)
				)
			)
		)
	)
)

;; remouve braces '{ }'
(defun removebraces (charlist)
	(cdr (removelast charlist))
)


;; remouve ' ' and transform char of
;; field-value in string
(defun reducestring (charlist container counter)
  	(cond 
		((and (null charlist) (null container)) nil)
    	((null charlist) (cons (liststring container) nil))
        ((eq (car charlist) #\")
	 		(reducestring (cdr charlist) container (+ 1 counter))
		)
        ((= counter 0)
	 		(cons 
				(car charlist)
	       		(reducestring (cdr charlist) container counter)
			)
		)
        ((= counter 1)
	 		(reducestring (cdr charlist)
			    (consend (car charlist) container)
			    counter
			)
		)
        ((= counter 2)
	 		(cons (liststring container)
	       		(reducestring charlist nil 0)
			)
		)        
        ((eq (car charlist) #\")
	 		(reducestring (cdr charlist)
			    (consend (car charlist) container)
			    (+ 1 counter)
			)
		)
        (T (error "Error!"))
	)
)

;; remouve ' ' and transform numbers of
;; field-value in string
(defun reducenumbers (charlist container)
  	(cond 
		((and (null charlist) (null container)) nil)
        ((null charlist) 
			(cons 
				(stringnumber (liststring container))
			    nil
			)
		)
        ((and (or 
					(eq (car charlist) #\,) 
					(eq (car charlist) #\}) 
					(eq (car charlist) #\])
				)
	      		(not (null container))
			)
         	(cons 
				(stringnumber (liststring container))
	       		(reducenumbers charlist nil)
			)
		)
        ((stringp (car charlist))
	 		(cons 
				(car charlist)
	       		(reducenumbers (cdr charlist) container)
			)
		) 
        ((digitelement (car charlist)) 
         	(reducenumbers (cdr charlist) (consend (car charlist) container))
		)
        ((or (null container) (null (digitelement (cadr charlist))))
         	(cons 
				(car charlist)
            	(reducenumbers (cdr charlist) container)
			)
		)
        (T (error "Number syntax error"))
	)
)


;; format numbers
;; "42.666" -> 32.666 
;; "420" ---> 420  
(defun stringnumber (string)
  	(if (null (find #\. string))
      	(parse-integer string)
    	(parse-float string)
	)
)


;; consend
;; put element end list
(defun consend (element l)
  	(if (null l)
    	(list element)
    	(cons 
			(first l) 
			(consend element (rest l))
		)
	)
)


;; check char is digit
(defun digitelement (element)
  	(or (and (> (char-int element) 47) (< (char-int element) 58)) 
        (eq element #\.) 
        (eq element #\-)
	 	(eq element #\+)
	)
)


;; remuove last element list
(defun removelast (l)
	(reverse (cdr (reverse l)))
)


;; jsonaccess
;; accept JSON obj and/or a list of field
;; give the matched obj
(defun jsonaccess (obj &optional field &rest fields)
  	(cond 
		((null field) obj)
		((null obj) nil)
		((and (eq 'jsonarray (first obj)) (null fields))
	 		(if (listp field)
	    		(searcharray (rest obj) (car field))
	   			(searcharray (rest obj) field)
			)
		)
		((and (eq 'jsonobj (first obj)) (null fields) (listp field))
	 		(jsonaccess obj (car field))
		)
		((and (eq 'jsonobj (first obj)) (null fields))
	 		(car (cdr (assoc field (rest obj) :test #'equal)))
		)
		((not (null fields))
	 		(cond 
				((listp (car fields))
					(if (numberp field)
		    			(getLevel (searcharray (cdr obj) field) (car fields))
		   				(getLevel 
							(car (cdr (assoc field (rest obj) :test #'equal)))
							(car fields)
						)
					)
				)
	       		(T (if (numberp field)
		      			(getLevel (searcharray (cdr obj) field) fields)
		    			(getLevel 
							(car (cdr (assoc field (rest obj) :test #'equal)))
							fields
						)
					)
				)
			)
		)
		(T (pprint "Error"))
	)
)


;; search for index in array
(defun searcharray (obj position)
  	(cond 
		((null obj) nil)
		((= 0 position) (car obj))
		(T (searcharray (cdr obj) (- position 1)))
	)
)


;; enter next level depth
(defun getLevel (obj fields)
  	(cond 
		((null obj) nil)
		((= (length fields) 1) (jsonaccess obj fields))
		((stringp (car fields))
	 		(jsonaccess 
				(car (cdr(assoc (car fields) (rest obj) :test #'equal)))
		   		(second fields)
		   		(cdr (cdr fields))
			)
		)
		(T (getLevel (searcharray (cdr obj) (car fields)) (cdr fields)))
	)
)
	

;; for jsondump, transform cons in string
(defun revertobj (jsonobj jsonstring)
  	(cond 
		((and (null jsonobj) (eq (char jsonstring 0) #\{))
			(car (list (concatenate 
						'string
				 		(string-right-trim ", " jsonstring) 
						"}"
					)
				)
			)
		)	
        ((null jsonobj) nil)
		((eq (car jsonobj) 'jsonarray)
	 		(revertarray jsonobj "")
		)
		((eq (car jsonobj) 'jsonobj)
	 		(revertobj (cdr jsonobj)
		    	(concatenate 
					'string 
					jsonstring 
					"{"
				)
			)
		)
		((ignore-errors (eq (car (car (cdr (car jsonobj)))) 'jsonobj))
	 		(revertobj (cdr jsonobj)
		     	(concatenate 
					'string 
					jsonstring
				  	"\"" 
					(car (car jsonobj)) 
					"\"" 
					" : "
				  	(revertobj (car (cdr (car jsonobj))) "")
				  	", "
				)
			)
		)
		((ignore-errors (eq (car (car (cdr (car jsonobj)))) 'jsonarray))
			(revertobj (cdr jsonobj)
		    	(concatenate 
					'string
					jsonstring
					"\"" 
					(car (car jsonobj)) 
					"\"" 
					" : "
				 	(revertarray (car (cdr (car jsonobj))) "")
				 	", "
				)
			)
		)
		(T (if (numberp (car (cdr (car jsonobj))))
	       		(revertobj (cdr jsonobj)
			   		(concatenate 
						'string 
						jsonstring
						"\"" 
						(car (car jsonobj)) 
						"\"" 
						" : "
						(write-to-string (car (cdr (car jsonobj))))
						", "
					)
				)
	     		(revertobj (cdr jsonobj)
			 		(concatenate 
						'string 
						jsonstring
				     	"\"" 
						(car (car jsonobj)) 
						"\"" 
						" : "
				    	"\"" 
						(car (cdr (car jsonobj))) 
						"\""
				      	", "
					)
				)
			)
		)
	)
)

;; for jsondump, transform array in string
(defun revertarray (jsonarray jsonstring)
  	(cond 
		((and (null jsonarray) (eq (char jsonstring 0) #\[))
	 		(car (list (concatenate 
						'string
				 		(string-right-trim ", " jsonstring) 
						"]"
					)
				)
			)
		)
		((null jsonarray) nil)
		((eq (car jsonarray) 'jsonarray)
			(revertarray 
				(cdr jsonarray) 
				(concatenate 
					'string 
					jsonstring 
					"["
				)
			)
		)
		((ignore-errors (eq (car (car jsonarray)) 'jsonobj))
	 		(revertarray (cdr jsonarray)
		       	(concatenate 
					'string 
					jsonstring
					(revertobj (car jsonarray) "") 
					", "
				)
			)
		)
		((ignore-errors (eq (car (car jsonarray)) 'jsonarray))
	 		(revertarray (cdr jsonarray)
		       	(concatenate 
					'string 
					jsonstring
					(revertarray (car jsonarray) "") 
					", "
				)
			)
		)
		(T (if (numberp (car jsonarray))
	       		(revertarray (cdr jsonarray)
			    	(concatenate 
						'string 
						jsonstring
						(write-to-string (car jsonarray))
					  	", "
					)
				)
	     		(revertarray (cdr jsonarray)
			   		(concatenate 
						'string 
						jsonstring
						"\"" 
						(car jsonarray) 
						"\"" 
						", "
					)
				)
			)
		)
	)
)


;; jsonread
(defun jsonread (filename)
  	(with-open-file (in filename
		    :if-does-not-exist :error
		    :direction :input
		)
  		(jsonparse (loadchar in))
	)
)


;; read every char one by one
(defun loadchar (inputstream)
  	(let ((json (read-char inputstream nil 'eof)))
    	(if (eq json 'eof) 
			""
      		(string-append json (loadchar inputstream))
		)
	)
)


;; jsondump
(defun jsondump (jsonobj filename)
  	(with-open-file (out filename
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create
		)
		(format out (revertobj jsonobj ""))
	)
)


;;;; end of file -- jsonparse.l