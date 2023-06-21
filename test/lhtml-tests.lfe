(defmodule lhtml-tests
 (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "include/lhtml.lfe")

;;; -----------
;;; library API
;;; -----------

(deftest all
 (is-equal #"<br/>" (html '(br)))
 (is-equal #"<br class='a'/>" (html '(br ((class "a")))))
 (is-equal #"<br class='a'/>" (html '(br ((class #"a")))))
 (is-equal #"<p>Testing</p>" (html '(p () "Testing")))
 (is-equal #"<p class='my-paragraph'>Testing</p>" (html '(p ((class "my-paragraph")) "Testing")))
 (is-equal #"<html><head><title>My title</title></head><body><p>My paragraph</p></body></html>" (html '(html () ((head () ((title () "My title")))
														 (body () ((p () "My paragraph"))))))))
