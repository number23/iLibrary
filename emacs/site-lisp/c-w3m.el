;; w3m
(when (eq system-type 'gnu/linux)
  (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (autoload 'w3m-search "w3m-search" "Search words using emacs-w3m." t)
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-command-arguments '("-cookie" "-F"))
  (setq w3m-use-cookies t)
  (setq w3m-home-page "http://www.google.com/")
  (require 'mime-w3m)
  (setq w3m-default-display-inline-image t)
  (setq w3m-default-toggle-inline-images t))

(provide 'c-w3m)
