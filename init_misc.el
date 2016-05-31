(defun translate-lingvo (word)
  "Translate a word with Yandex's Lingvo"
  (interactive "sWord: ")
  (eww;;w3m-browse-url
   (concatenate 'string
                "https://lingvolive.ru/translate/en-ru/" word)))
