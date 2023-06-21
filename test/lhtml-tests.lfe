(defmodule lhtml-tests
 (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;;; -----------
;;; library API
;;; -----------

(deftest all
 (is-equal #"<br/>" (lhtml:html '(br)))
 (is-equal #"<br class='a'/>" (lhtml:html '(br ((class "a")))))
 (is-equal #"<br class='a'/>" (lhtml:html '(br ((class #"a")))))
 (is-equal #"<p>Testing</p>" (lhtml:html '(p () "Testing")))
 (is-equal #"<p class='my-paragraph'>Testing</p>" (lhtml:html '(p ((class "my-paragraph")) "Testing")))
 (is-equal #"<html><head><title>My title</title></head><body><p>My paragraph</p></body></html>" (lhtml:html '(html () ((head () ((title () "My title")))
														 (body () ((p () "My paragraph"))))))))
