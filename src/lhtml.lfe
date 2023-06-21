(defmodule lhtml
 (export
  (html 1)
  (parse 1)
  (parse-attr 1)))

;;; -----------
;;; library API
;;; -----------

(defun parse (html)
 (build-tree html () ()))

(defun build-tree 
 ([html () ()] (let* (((list _ tail) (binary:split html #"<"))
		      (body (case (get-openning-tag tail)
				  ((list 'no-attrs tag tail1) (build-body tag tail1))
				    ((list 'openning-tag tag tail1) (build-attr-body tag tail1)))))
				      body)))

(defun get-openning-tag (html)
 (get-openning-tag html ()))

(defun get-openning-tag 
 ([(binary " />" (else binary)) acc] `(,acc ,else))
 ([(binary "/>" (else binary)) acc] `(,acc ,else))
 ([(binary ">" (else binary)) acc] `(no-attrs ,acc ,else))
 ([(binary " " (else binary)) acc] `(openning-tag ,acc ,else))
 ([html acc] (get-openning-tag (binary:part html 1 (-  (size html) 1)) (++ acc (list (binary:first html))))))

(defun build-body (tag html)
 (let (((list body _) (binary:split html (list_to_binary `("</" ,tag ">")))))
  body))

(defun build-attr-body (tag html)
 (let* (((list attr-bin else) (binary:split html #">"))
	(attr-list (parse-attr attr-bin))
        ((list body tail) (binary:split else (list_to_binary `("</" ,tag ">")))))
  `(,tag ,attr-list ,body)))


(defun parse-attr (attrs)
 (if (== (string:find attrs "=") 'nomatch)
      (string:split attrs " " 'all)
       (parse-attr attrs () ())))

(defun parse-attr 
 ([() _ attrs-list] attrs-list)
 ([attrs () acc] (let (((list head tail) (cl:remove #"" (string:split attrs "=")))) 
		 (parse-attr tail (list head ()) acc)))
 ([attrs (list k v) acc] (case (cl:remove #"" (string:split attrs " "))
			       ((list head tail) (if (== (string:find head "=") 'nomatch)
						      (parse-attr tail `(,k ,(list_to_binary `(,v " " ,head))) acc)
						     (let (((list head1 tail1) (cl:remove #"" (string:split head "="))))
						      (parse-attr tail `(,head1 ,tail1) (cons `(,k ,v) acc)))))
			       (_ (case (cl:remove #"" (string:split attrs "="))
					    ((list head tail) (parse-attr tail (list head ()) (cons (list k (string:trim v)) acc)))
					    (_ (cons (list k (string:trim (list_to_binary `(,v " " ,attrs)))) acc)))))))

(defun build-attr
 ([(list attr)] (list_to_binary (list " " (atom_to_binary attr))))
 ([(list attr value)] (list_to_binary (list " " (atom_to_binary attr) "='" value "'"))))

(defun html (data)
 (case (cl:type-of (car data))
       ('atom
	(case data
	      ((list tag) (list_to_binary (list "<" (atom_to_binary tag) "/>")))
	      ((list tag attrs) (list_to_binary (list "<" (atom_to_binary tag)  
						 (cl:mapcar (lambda (attr) (build-attr attr)) attrs) "/>")))
	      ((list tag attrs body) (list_to_binary (list "<" (atom_to_binary tag)
						      (cl:mapcar (lambda (attr) (build-attr attr)) attrs) ">" 
						      (if (clj:binary? body) 
							  body
							  (if (clj:integer? (car body))
							      body
							      (html body)))
						      "</" (atom_to_binary tag) ">")))))
       ('list (list_to_binary (cl:mapcar (lambda (elem) (html elem)) data)))))
