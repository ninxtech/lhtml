(defmodule lhtml
 (export
  (html 1)
  (get-openning-tag 1)
  (parse 1))
 (export all))

;;; -----------
;;; library API
;;; -----------

(defun parse (html)
 (build-tree html () ()))

(defun singleton? (tag)
 "detects if a tag is a singleton"
 (cl:member tag (list "area" "base" "br" "col" "command" "embed" "hr" "img" "input" "link" "meta" "param" "source" "!DOCTYPE" "!doctype")))

(defun build-tree 
 "builds the html tree"
 ([#"" () acc]  (cl:remove () acc))
 ([#"" part acc] (cons part acc))
 ([(binary "<" (tail binary)) part acc] (let* (((list element else) (case (get-openning-tag tail)
									  ((list 'script _ tail) (let (((list script-body other) (build-body "script" tail)))
									    `(("script" () ,script-body) ,other)))
									  ((list 'script-attrs _ tail) (build-script-attr-body "script" tail))
									    ((list 'no-attrs tag tail1) (if (singleton? tag)
									      `((,tag () ()) ,tail1)
									      (let (((list tag-body other) (build-body tag tail1)))
									       `((,tag () ,(parse tag-body)) ,other))))
									    ((list 'openning-tag tag tail1) (if (singleton? tag)
									      (build-attr tag tail1)
									      (build-attr-body tag tail1)))
									    ((list 'single-tag tag tail1) `((,tag () ()) ,tail1)))))
									      (build-tree else () (cons element (cons part acc)))))
 ([html part acc] (build-tree (binary:part html 1 (- (size html) 1)) (++ part (list (binary:first html))) acc)))

(defun get-openning-tag (html)
 (get-openning-tag html ()))

(defun get-openning-tag 
 " gets the opening tag depending on what comes first of /> >"
 ([(binary "script>" (else binary)) acc] `(script () ,else))
 ([(binary "script >" (else binary)) acc] `(script () ,else))
 ([(binary "script" (else binary)) acc] `(script-attrs () ,else))
 ([(binary " />" (else binary)) acc] `(single-tag ,acc ,else))
 ([(binary "/>" (else binary)) acc] `(single-tag ,acc ,else))
 ([(binary ">" (else binary)) acc] `(no-attrs ,acc ,else))
 ([(binary " >" (else binary)) acc] `(no-attrs ,acc ,else))
 ([(binary " " (else binary)) acc] `(openning-tag ,acc ,else))
 ([html acc] (get-openning-tag (binary:part html 1 (-  (size html) 1)) (++ acc (list (binary:first html))))))

(defun build-body (tag html)
 " builds tags with no attributes, only body"
 (io:format "~p~n" (list (list tag html)))
 (let (((list body else) (binary:split html (list_to_binary `("</" ,tag ">")))))
  `(,body ,else)))

(defun build-attr (tag html)
 "builds attributes for singletons, checks what comes first of /> and > and uses that the closing tag, no body"
 (let* (((list attr-> else->) (binary:split html #">"))
	((list attr-/> else-/>) (case (binary:split html #"/>") ((= (list _ _) v) v) (_ (list () ()))))
	((list attr-bin else) (if (== () attr-/>)
				  (list attr-> else->)
				  (if (> (size attr->) (size attr-/>))
				      (list attr-/> else-/>)
				      (list attr-> else->))))
	(attr-list (parse-attr attr-bin)))
  `((,tag ,attr-list ()) ,else)))


(defun build-script-attr-body (tag html)
 " builds attributes and body for 'doubletons'"
 (let* (((list attr-bin else) (binary:split html #">"))
	(attr-list (parse-attr attr-bin))
	((list body tail) (binary:split else (list_to_binary `("</" ,tag ">")))))
  `((,tag ,attr-list (,body)) ,tail)))

(defun build-attr-body (tag html)
 " builds attributes and body for 'doubletons'"
 (io:format "~p~n" (list (list tag html)))
 (let* (((list attr-bin else) (binary:split html #">"))
	(attr-list (parse-attr attr-bin))
	((list body tail) (binary:split else (list_to_binary `("</" ,tag ">")))))
  `((,tag ,attr-list ,(parse body)) ,tail)))

(defun parse-attr (attrs)
 "parses the attributes and returns a list"
 (if (== (string:find attrs "=") 'nomatch)
      (string:split attrs " " 'all)
       (parse-attr attrs () ())))

(defun parse-attr 
 "does the actual passing, returning the kv pairs for attributes"
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
