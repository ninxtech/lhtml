(defmodule lhtml-tests
 (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;;; -----------
;;; library API
;;; -----------

(deftest html
 (is-equal #"<br/>" (lhtml:html '(br)))
 (is-equal #"<br class='a'/>" (lhtml:html '(br ((class "a")))))
 (is-equal #"<br class='a'/>" (lhtml:html '(br ((class #"a")))))
 (is-equal #"<p>Testing</p>" (lhtml:html '(p () "Testing")))
 (is-equal #"<p class='my-paragraph'>Testing</p>" (lhtml:html '(p ((class "my-paragraph")) "Testing")))
 (is-equal #"<html><head><title>My title</title></head><body><p>My paragraph</p></body></html>" (lhtml:html '(html () ((head () ((title () "My title")))
														 (body () ((p () "My paragraph"))))))))

(deftest parse
(is-equal `(("br" () ())) (lhtml:parse #"<br>"))
(is-equal `(("br" () ())) (lhtml:parse #"<br/>"))
(is-equal `(("br" ((#"class" #"'a'")) ())) (lhtml:parse #"<br class='a'/>"))
(is-equal `("a") (lhtml:parse #"a"))
(is-equal `(("div" () (("p" () ("test"))))) (lhtml:parse #"<div><p>test</p></div>"))
)

(deftest get-by-tag 
(is-equal `(("div" () ())) (html:get-by-tag #"<div></div>"))
(is-equal `(("div" () ())) (html:get-by-tag #"<div></div>"))
)
