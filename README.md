
# lhtml
lhtml is an LFE library to work with html. 
## Goals
- [x] To generate html, like YAWS' EHTML or CL's cl-who
- [x] To parse html
- [x] To search html tree by tag name and by attribute value
## Generating html
We use S-expressions to generate html.
### Single tags
```LFE
lfe> (lhtml:html '(br))
#"<br/>"
```
### Single tags with attributes
Attribute values can be strings or binaries.
```LFE
lfe> (lhtml:html '(br ((class "a"))))
#"<br class='a'/>"

lfe> (lhtml:html '(br ((class #"a"))))
#"<br class='a'/>"
```
### With body
```LFE
lfe> (lhtml:html '(p () "Testing"))
#"<p>Testing</p>"

lfe> (lhtml:html '(p ((class "my-paragraph")) "Testing"))
#"<p class='my-paragraph'>Testing</p>"
```
### A list of tags
```LFE
lfe> (lhtml:html '(html () (
			(head () ((title () "My title")))
			(body () ((p () "My paragraph")))
		)))
#"<html><head><title>My title</title></head><body><p>My paragraph</p></body></html>"
```

# Parsing
```LFE
lfe> (lhtml:parse #"<div></div>")
(("div" () ()))
lfe> (lhtml:parse #"<div a='a'></div>")
(("div" ((#"a" #"'a'")) ()))
lfe> (lhtml:parse #"<div>a</div>")
(("div" () ("a")))
```

# Searching
Get by tag name, `tag` is a string, function can't get script tag.
```LFE
(lhtml:get-by-tag "tag" tree)
```
Get by attribute value. `attr` and `value` are binaries.
```LFE
(lhtml:get-by-attr-value attr value tree)
```
Get all scripts in a tree.
```LFE
(lhtml:get-scripts tree)
```
### Tests
Tests are in `tests/lhtml-tests.lfe`.
To run them: 
```LFE
rebar3 as test lfe ltest
```
