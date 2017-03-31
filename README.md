# advance-words-count.el
Extended `count-words` function

```emacs-lisp 
(require 'advance-words-count) 
``` 

Use `advance-words-count` to display a words count message in minibuffer. If an
universal-argument is used, display the message verbosely.

This package is intended to be a replacement of Emacs's original `count-words`
function. It uses regexp to match words and characters of any language, which
can be set in `words-count-regexp-list`.

You can define your own function to display messages, and set to
`words-count-messages-func` to your perfered function. For an easier way, use
`words-count-define-func` to define rhe function.

Example:
```emacs-lisp
(words-count-define-func " %d %d" ((cadr list) (car list)) t)
```

Set `(setq words-count-messages-display 'pos-tip)` to uses `pos-tip`.
