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
