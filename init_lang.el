(my-load-init '(lisp cpp))

(setq
 case-fold-search t
 column-number-mode t
 tags-revert-without-query t)

(my-require-and-eval (nxml-mode nxml))

(my-require-and-eval (cmake-mode nil cmake-mode))
