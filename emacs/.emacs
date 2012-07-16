(set-language-environment 'Chinese-GB)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default pathname-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(setq ansi-color-for-comint-mode t)

(global-set-key (kbd "C-SPC") 'nil)
(setq default-directory "~/")
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)

(setq backup-directory-alist '(("." . "~/backups")))
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 3)
(setq delete-old-versions t)
(setq backup-by-copying t)
(setq auto-save-default nil)

(setq user-full-name "N23")
(setq user-mail-address "number23.cn@gmail.com")

(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28))
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(set-scroll-bar-mode nil)

(when window-system
  (if (eq system-type 'darwin)
      (progn
        (setq epg-gpg-program "/usr/local/bin/gpg")
        (setq ns-pop-up-frames nil)
        ;;(set-frame-font "Monaco:style=Roman:size=16")
        (set-frame-font "YaHei Consolas Hybrid:size=16")))

  (if (eq system-type 'gnu/linux)
      (progn
        (tool-bar-mode nil)
        ;;(set-default-font "YaHei Consolas Hybrid-12")
        (set-default-font "Consolas-12:bold"))))

(setq default-major-mode 'text-mode)
(setq default-fill-column 80)
(show-paren-mode t)
(column-number-mode t)
(setq show-paren-style 'parenthesis)
(setq frame-title-format "emacs@%b")
(setq global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(setq font-lock-global-modes '(not shell-mode text-mode))
(setq font-lock-verbose t)
(setq font-lock-maximum-size '((t . 1048576) (vm-mode . 5250000)))
(setq auto-image-file-mode t)
(setq x-select-enable-clipboard t)
(setq mouse-yank-at-point t)
(setq scroll-margin 3 scroll-conservatively 10000)
(setq enable-recursive-minibuffers t)
(setq kill-ring-max 200)
(setq-default require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-to-list 'load-path' "~/.emacs.d/site-lisp")
(global-auto-revert-mode t)
(setq vc-follow-symlinks t)

(when window-system
  (load "server")
  (unless (server-running-p) (server-start)))

(require 'c-w3m)
(require 'c-ibus)

;;; recentf
(recentf-mode t)
(setq recentf-max-saved-items 30)
(setq recentf-auto-cleanup 300)
(setq recentf-save-file "~/.emacs.d/recentf-list")

;;; package.el
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(color-theme-tangotango
                      paredit
                      highlight-parentheses
                      slime
                      slime-repl
                      clojure-mode
                      clojurescript-mode
                      js2-mode
                      markdown-mode
                      ac-slime
                      auto-complete))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'color-theme-tangotango)
(color-theme-tangotango)

;;; slime
;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;(setq inferior-lisp-program "/usr/local/bin/ccl")
;(setq common-lisp-hyperspec-root "http://127.0.0.1/docs/HyperSpec-7-0/HyperSpec/")

(defun lisp-indent-or-complete (&optional arg)
  (interactive "P")
  (if (or (looking-back "^\\s-*") (bolp))
      (call-interactively 'lisp-indent-line)
      (call-interactively 'slime-indent-and-complete-symbol)))

(eval-after-load "lisp-mode"
  '(progn
     (define-key lisp-mode-map (kbd "TAB") 'lisp-indent-or-complete)))

(add-hook 'lisp-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

(eval-after-load 'slime '(setq slime-protocol-version 'ignore))
(setq slime-autodoc-use-multiline-p t)
(setq slime-repl-history-remove-duplicates t)
(setq slime-repl-history-trim-whitespaces t)
(setq slime-net-coding-system 'utf-8-unix)
(require 'c-slime-autodoc)

;;; python-mode
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist(cons '("python" . python-mode)
                             interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
;;copy pycomplete.py Pymacs into PYTHONPATH
(setq py-python-command "/usr/local/bin/pypy")
(setq-default py-indent-offset 4)
(setq pymacs-python-command py-python-command)
(eval-after-load "python-mode"
  '(progn
     (autoload 'pymacs-apply "pymacs")
     (autoload 'pymacs-call "pymacs")
     (autoload 'pymacs-load "pymacs" nil t)
     (autoload 'pymacs-exec "pymacs" nil t)
     (autoload 'pymacs-eval "pymacs" nil t)
     (require 'pycomplete)))

;;; js-mode
;; emacs -batch -f batch-byte-compile *.el
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(require 'highlight-parentheses)
(dolist (mode '(clojure clojurescript js2 python scheme emacs-lisp lisp))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            (lambda ()
              (paredit-mode t)
              (highlight-parentheses-mode t)
              (hs-minor-mode t))))
(setq hl-paren-colors
      '("red1" "orange1" "yellow1" "green1" "cyan1"
        "slateblue1" "magenta1" "purple"))

(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u0192") nil))))))
(add-hook 'clojure-mode-hook 'esk-pretty-fn)
(add-hook 'clojurescript-mode-hook 'esk-pretty-fn)

;;; M-x inferior-lisp
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq safe-local-variable-values
                  '((inferior-lisp-program . "lein repl")
                    (inferior-lisp-program . "smvn clojure:repl")))))

;;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (append
       '(("\\.md$"  . markdown-mode)
         ("\\.markdown$"  . markdown-mode))
       auto-mode-alist))

;;; EasyPG
(require 'epa-file)
(epa-file-enable)

;;; auto-complete: M-n, M-p, C-g
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict/")
(ac-config-default)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;;; colored REPL
(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (require 'clojure-mode)
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))

;;; misc functions
(defun copy-lines (&optional arg)
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (next-line arg)
    (kill-ring-save (mark) (point))))

(defun switch-buffer-scratch ()
  "Switch to the scratch buffer. If the buffer doesn't exist"
  (interactive)
  (let* ((scratch-buffer-name "*scratch*")
         (scratch-buffer (get-buffer scratch-buffer-name)))
    (unless scratch-buffer
      (setq scratch-buffer (get-buffer-create scratch-buffer-name))
      (with-current-buffer scratch-buffer
        (lisp-interaction-mode)))
    (switch-to-buffer scratch-buffer)))

(defun goto-match-parenthesis (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "P")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

(global-set-key (kbd "C-c C-w") 'copy-lines)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key "\C-cbs" 'switch-buffer-scratch)
(global-set-key (kbd "C-%") 'goto-match-parenthesis)
