# lhtml
lhtml is an LFE library to work with html. 
## Goals
- [x] To generate html, like YAWS' EHTML or CL's cl-who
- [ ] To parse html
## Generating html
Include the lhtml.lfe file.
```LFE
lfe> (include-lib "lhtml/include/lhtml.lfe")
```
We use S-expressions to generate html.
### Single tags
```LFE
lfe> (html '(br))
#"<br/>"
```
### Single tags with attributes
Attribute values can be strings or binaries.
```LFE
lfe> (html '(br ((class "a"))))
#"<br class='a'/>"

lfe> (html '(br ((class #"a"))))
#"<br class='a'/>"
```
### With body
```LFE
lfe> (html '(p () "Testing"))
#"<p>Testing</p>"

lfe> (html '(p ((class "my-paragraph")) "Testing"))
#"<p class='my-paragraph'>Testing</p>"
```
### A list of tags
```LFE
lfe> (html '(html () (
						(head () ((title () "My title")))
						(body () ((p () "My paragraph")))
					)))
#"<html><head><title>My title</title></head><body><p>My paragraph</p></body></html>"
```

### Tests
Tests are in `tests/lhtml-tests.lfe`.
To run them: 
```LFE
rebar3 as test lfe ltest
```
