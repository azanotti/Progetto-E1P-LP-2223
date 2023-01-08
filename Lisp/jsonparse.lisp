;;;; Federico Combi 886034
;;;; Alessandro Zanotti 885892

;;;; -*- Mode: Lisp -*-
;;;; jsonparse.lisp


;;; jsonparse
;;; Accepts a string as input and produces a structure similar to the one
;;; illustrated for making Prolog
(defun jsonparse (JSONString)
  (let ((JSONList (sanitizeCharlist(coerce JSONString 'list))))
    (or (identifyObject JSONList)
        (identifyArray JSONList)
        (error "ERROR: syntax error"))))

;;; jsonaccess
;;; Takes a JSON object and an array of "fields", retrieves the corresponding
;;; object.
;;; A field represented by N (where N is a number greater than or equal to 0)
;;; represents an index of a JSON array.
(defun jsonaccess (object &optional field &rest fields)
  (cond ((null field) object)
	((null object) nil)
	((and (eq 'jsonarray (first object))
	      (null fields))
	 (if (listp field)
	     (searchArray (rest object) (car field))
	     (searchArray (rest object) field)))
	((and (eq 'jsonobj (first object))
	      (null fields)
	      (listp field))
	 (jsonaccess object (car field)))
	((and (eq 'jsonobj (first object))
	      (null fields))
	 (car (cdr (assoc field (rest object) :test #'equal))))
	((not (null fields))
	 (cond ((listp (car fields))
		(if (numberp field)
		    (getLevel (searchArray (cdr object) field) (car fields))
		    (getLevel (car (cdr (assoc field (rest object) :test #'equal)))
			      (car fields))))
	       (T (if (numberp field)
		      (getLevel (searchArray (cdr object) field) fields)
		      (getLevel (car (cdr (assoc field (rest object) :test #'equal)))
				fields)))))
	(T (pprint "Error!"))))

;;; jsonread
;;; Opens file filename returns a JSON object (or throws an error)
(defun jsonread (filename)
  (with-open-file (in filename
		      :if-does-not-exist :error
		      :direction :input)
    (jsonparse (loadChar in))))

;;; jsondump
;;; Writes the JSON object to file filename in JSON syntax.
;;; If filename does not exist, it is created.
;;; If it exists it is overwritten.
(defun jsondump (jsonobj filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (format out (lispObjToJsonString jsonobj ""))))


;;; dependencies jsonparse function
;;; sanitizeCharlist
;;; Remove the chars that create problems at the pars
(defun sanitizeCharlist (list)
  (remove #\Return
	  (remove #\Tab
		  (remove #\Newline
			  (remove #\Space
				  (substitute #\" #\' list))))))

;;; identifyObject
;;; If the first element of the list is '{' and the second element of the list
;;; is '}' then the object is jsonobj.
;;; Otherwise use the multiple-value-bind macro which use identifyMember and
;;; at result associates the member, if what is associated with other_objects
;;; is a '}' then the object is (jsonobj result).
;;; Otherwise report an error.
(defun identifyObject (object)
  (if (eql (car object) #\{)
      (if (eql (second object) #\})
          (values (list 'jsonobj) (cdr (cdr object)))
          (multiple-value-bind (result other_objects)
              (identifyMember (cdr object))
            (if (eql (car other_objects) #\})
		(values (cons 'jsonobj result) (cdr other_objects))
		(error "ERROR: syntax error"))))
      (values nil object)))

;;; identifyArray
;;; If the first element of the list is a '[' e the second element of the
;;; list is a ']' then the object is jsonarray.
;;; Otherwise use the multiple-value-bind macro which use identifyElement
;;; and at result associates the element, if what is associated with
;;; other_arrays is a ']' then the object is (jsonarray result).
;;; Otherwise report an error.
(defun identifyArray (array)
  (if (eql (car array) #\[)
      (if (eql (second array) #\])
          (values (list 'jsonarray) (cdr (cdr array)))
          (multiple-value-bind (result other_arrays)
              (identifyElement (cdr array))
            (if (eql (car other_arrays) #\])
		(values (cons 'jsonarray result) (cdr other_arrays))
		(error "ERROR: syntax error"))))
      (values nil array)))

;;; identifyMember
;;; Use the multiple-value-bind macro which use identifyPair and at result
;;; associates the value, if the head of the one associated with
;;; other_members is a ',' then reuse the multiple-value-bind macro which
;;; use identifyMember and at result_other_members associates the member.
;;; The member consists of result (pair) and result_other_members (member).
(defun identifyMember (member)
  (multiple-value-bind (result other_members)
      (identifyPair member)
    (if (null result)
        (error "ERROR: syntax error")
	(if (eql (car other_members) #\,)
            (multiple-value-bind (result_other_members rest_other_members)
		(identifyMember (cdr other_members))
              (if (null result_other_members)
                  (error "ERROR: syntax error")
		  (values (cons result result_other_members) rest_other_members)))
            (values (cons result nil) other_members)))))

;;; identifyElement
;;; Use the multiple-value-bind macro which use identifyValue and at result
;;; associates the value, if the head of what is associated with
;;; other_elements is a ',' then reuse the multiple-value-bind macro which
;;; use identifyElement and at result_other_elements associates the element.
;;; The element consists of result (value) and result_other_elements (element).
(defun identifyElement (element)
  (multiple-value-bind (result other_elements)
      (identifyValue element)
    (if (null result)
        (error "ERROR: syntax error")
	(if (eql (car other_elements) #\,)
            (multiple-value-bind (result_other_elements rest_other_elements)
		(identifyElement (cdr other_elements))
              (if (null result_other_elements)
                  (error "ERROR: syntax error")
		  (values (cons result result_other_elements) rest_other_elements)))
            (values (cons result nil) other_elements)))))

;;; identifyPair
;;; Use the multiple-value-bind macro which use identifyString and at result
;;; matches the string, and if the head of the one mapped to other_pairs 
;;; matches ':' then reuse the multiple-value-bind macro which use 
;;; identifyValue and atresult_other_pairs associates the pair.
;;; The pair consists of result (string) and result_other_pairs (pair).
(defun identifyPair (pair)
  (multiple-value-bind (result other_pairs)
      (identifyString pair)
    (if (or (null result)
            (null other_pairs))
        (error "ERROR: syntax error")
	(if (eql (car other_pairs) #\:)
            (multiple-value-bind (result_other_pairs rest_other_pairs)
		(identifyValue (cdr other_pairs))
              (if (null result_other_pairs)
                  (error "ERROR: syntax error")
		  (values (list result result_other_pairs) rest_other_pairs)))
            (error "ERROR: syntax error")))))

;;; identifyValue
;;; If the first element is an '{' then call identifyObject with value,
;;; otherwise if it is an '[' then call identifyArray with value,
;;; otherwise if it is ' ' ' or ' " ' then  call identifyString with value,
;;; otherwise if it is a '+' or a '-' or a number (digit-char-p) then call
;;; identifyNumber with value.
;;; Otherwise  report an error.
(defun identifyValue (value)
  (cond ((eql (car value) #\{)
         (identifyObject value))
        ((eql (car value) #\[)
         (identifyArray value))
        ((or (eql (car value) #\")
             (eql (car value) #\'))
         (identifyString value))
        ((or (eql (car value) #\+)
             (eql (car value) #\-)
             (digit-char-p (car value))) (identifyNumber value))
        (T (error "ERROR: syntax error"))))

;;; identifyString
;;; If the first element of the string begins with a ' " ' then use the
;;; multiple-value-bind macro which use identifyAnyChars and at result
;;; associates the string, in the form of a list, which is transformed,
;;; thanks to coerce, into a string.
;;; Otherwise report an error.
(defun identifyString (string)
  (cond ((eql (car string) #\")
         (multiple-value-bind (result other_strings)
             (identifyAnyChars (cdr string) (car string))
           (values (coerce result 'string) other_strings)))
        (T (error "ERROR: syntax error"))))

;;; identifyAnyChars
;;; If the first element of the string is equal to ' " ' (end) then done
;;; parsing the string.
;;; Otherwise if the head is an ascii character then use the
;;; multiple-value-bind macro which use identifyAnyChars and at result
;;; matches the char itself which is concatenated with the previous ascii
;;; character (car chars).
;;; Otherwise report an error.
(defun identifyAnyChars (chars end)
  (if (eql (car chars) end) (values nil (cdr chars))
      (if (and (<= (char-int (car chars)) 128)
               (<= (char-int (car chars)) 128))
          (multiple-value-bind (result other_chars)
              (identifyAnyChars (cdr chars) end)
            (values (cons (car chars) result) other_chars))
	  (error "ERROR: syntax error"))))

;;; identifyNumber
;;; If the first element is a '-' or a '+' or a number (digit-char-p) then use
;;; the multiple-value-bind macro which use identifyInteger and at result
;;; associates the number.
;;; When done parsing the number turn it into a string.
;;; Otherwise report an error.
(defun identifyNumber (number)
  (cond ((or (eql (car number) #\-)
             (eql (car number) #\+)
             (digit-char-p (car number)))
         (multiple-value-bind (result other_numbers)
             (identifyInteger (cdr number))
           (values (car (multiple-value-list
                         (read-from-string
                          (coerce
                           (cons (car number) result) 'string)))) 
		   other_numbers)))
        (T (error "ERROR: syntax error"))))

;;; identifyInteger
;;; If the first element is an integer (digit-char-p) then use the
;;; multiple-value-bind macro which use identifyInteger and associates the
;;; number to result which is concatenated with the previous integer (car
;;; integer).
;;; If the first element is a point then use the multiple-value-bind macro
;;; which use to identifyFloat and a result associates the decimal part of the
;;; number which is concatenated with the non-decimal part of the number (car
;;; integer).
(defun identifyInteger (integer)
  (if (null (car integer)) nil
      (cond ((and (eql (car integer) #\.)
                  (digit-char-p (second integer)))
             (multiple-value-bind (result other_integers)
		 (identifyFloat (cdr integer))
               (values (cons (car integer) result) other_integers)))
            ((digit-char-p (car integer))
             (multiple-value-bind (result other_integers)
		 (identifyInteger (cdr integer))
               (values (cons (car integer) result) other_integers)))
            (T (values nil integer)))))

;;; identifyFloat
;;; If the first element is an integer (digit-char-p) then use the
;;; multiple-value-bind macro which use to identifyFloat and a result maps the
;;; integer which is concatenated with the previous integer (car float).
(defun identifyFloat (float)
  (if (null (car float)) nil
      (if (digit-char-p (car float))
          (multiple-value-bind (result other_floats)
              (identifyFloat (cdr float))
            (values (cons (car float) result) other_floats))
	  (values nil float))))


;;; dependencies jsonaccess function


;;; searchArray
;;; Serch with index in the array.
;;; If array dosn't exist or index is not valid report an error.
(defun searchArray (array position)
  (cond ((null array) (pprint "Error: out of bounds"))
	((= 0 position) (car array))
	(T (searchArray (cdr array) (- position 1)))))

;;; getLevel
;;; Enter in the next Level of an obj
(defun getLevel (obj fields)
  (cond ((null obj) nil)
	((= (length fields) 1) (jsonaccess obj fields))
	((stringp (car fields))
	 (jsonaccess (car (cdr
			   (assoc (car fields) (rest obj) :test #'equal)))
		     (second fields)
		     (cdr (cdr fields))))
	(T (getLevel (searchArray (cdr obj) (car fields)) (cdr fields)))))


;;; dependencies jsonread function


;;; loadChar
;;; Read every char of an inputstream one by one
(defun loadChar (inputstream)
  (let ((json (read-char inputstream nil 'eof)))
    (if (eq json 'eof) ""
	(string-append json (loadChar inputstream)))))


;;; dependencies jsondump function


;;; lispObjToJsonString
;;; Transform from Common Lisp syntax to JSON, build recursively a string
;;; (using concatenate) using an accumulator and return it in the base case.
;;; Object case
(defun lispObjToJsonString (j_object jsonstring)
  (cond ((and (null j_object) (eq (char jsonstring 0) #\{))
	 (car (list (concatenate 'string
				 (string-right-trim
				  ", " jsonstring) "}"))))
        ((null j_object) nil)
	((eq (car j_object) 'jsonarray)
	 (lispArrayToJsonString j_object ""))
	((eq (car j_object) 'jsonobj)
	 (lispObjToJsonString (cdr j_object)
			      (concatenate 'string jsonstring "{")))
	((ignore-errors (eq (car (car (cdr (car j_object)))) 'jsonobj))
	 (lispObjToJsonString (cdr j_object)
			      (concatenate 'string jsonstring
					   "\"" (car (car j_object)) "\"" " : "
					   (lispObjToJsonString (car (cdr (car j_object))) "")
					   ", ")))
	((ignore-errors (eq (car (car (cdr (car j_object)))) 'jsonarray))
	 (lispObjToJsonString (cdr j_object)
			      (concatenate 'string jsonstring
					   "\"" (car (car j_object)) "\"" " : "
					   (lispArrayToJsonString (car (cdr (car j_object))) "")
					   ", ")))
	(T (if (numberp (car (cdr (car j_object))))
	       (lispObjToJsonString (cdr j_object)
				    (concatenate 'string jsonstring
						 "\"" (car (car j_object)) "\"" " : "
						 (write-to-string (car (cdr (car j_object))))
						 ", "))
	       (lispObjToJsonString (cdr j_object)
				    (concatenate 'string jsonstring
						 "\"" (car (car j_object)) "\"" " : "
						 "\"" (car (cdr (car j_object))) "\""
						 ", "))))))

;;; lispArrayToJsonString
;;; Transform from Common Lisp syntax to JSON, build recursively a string
;;; (using concatenate) using an accumulator and return it in the base case.
;;; Array case.
(defun lispArrayToJsonString (j_array jsonstring)
  (cond ((and (null j_array) (eq (char jsonstring 0) #\[))
	 (car (list (concatenate 'string
				 (string-right-trim ", " jsonstring) "]"))))
	((null j_array) nil)
	((eq (car j_array) 'jsonarray)
	 (lispArrayToJsonString (cdr j_array) 
				(concatenate 'string jsonstring "[")))
	((ignore-errors (eq (car (car j_array)) 'jsonobj))
	 (lispArrayToJsonString (cdr j_array)
				(concatenate 'string jsonstring
					     (lispObjToJsonString (car j_array) "") ", ")))
	((ignore-errors (eq (car (car j_array)) 'jsonarray))
	 (lispArrayToJsonString (cdr j_array)
				(concatenate 'string jsonstring
					     (lispArrayToJsonString (car j_array) "") ", ")))
	(T (if (numberp (car j_array))
	       (lispArrayToJsonString (cdr j_array)
				      (concatenate 'string jsonstring
						   (write-to-string (car j_array))
						   ", "))
	       (lispArrayToJsonString (cdr j_array)
				      (concatenate 'string jsonstring
						   "\"" (car j_array) "\"" ", "))))))

;;;; end of file -- jsonparse.lisp --
