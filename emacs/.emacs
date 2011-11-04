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

;;(setq make-backup-files nil)
;;(setq-default make-backup-files nil)
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
(setq tab-stop-list '(4 8 12 16))
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(set-foreground-color "grey")
(set-background-color "black")
(set-cursor-color "gold1")
(set-mouse-color "gold1")

(set-scroll-bar-mode nil)
;;(customize-set-variable 'scroll-bar-mode 'right)

(when window-system
  (if (eq system-type 'darwin)
      (progn
        (setq ns-pop-up-frames nil)
        (set-frame-font "Monaco:style=Roman:size=16")))

  (if (eq system-type 'gnu/linux)
      (progn
        (tool-bar-mode nil)
        ;;  (set-default-font "YaHei Consolas Hybrid-12")
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

(when window-system
  (require 'color-theme)
  (color-theme-classic)
;;(require 'calendar-setting)
  (server-start)
)

;; slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/ccl")
(setq common-lisp-hyperspec-root "http://127.0.0.1/docs/HyperSpec-7-0/HyperSpec/")
(if window-system
  (slime))

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

(setq slime-autodoc-use-multiline-p t)
(setq slime-repl-history-remove-duplicates t)
(setq slime-repl-history-trim-whitespaces t)

;; python-mode
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist(cons '("python" . python-mode)
                             interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
;;(setq py-python-command "python")
;;copy pycomplete.py Pymacs into PYTHONPATH
(setq py-python-command "/usr/local/bin/pypy")
(setq-default py-indent-offset 4)
(setq pymacs-python-command py-python-command)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(require 'pycomplete)

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

(when window-system
;; ibus
  (when (eq system-type 'gnu/linux)
    (require 'ibus)
    (add-hook 'after-init-hook 'ibus-mode-on)
    (ibus-define-common-key ?\C-\s nil)
    (ibus-define-common-key ?\C-/ nil)
    (ibus-define-common-key ?\S-\s nil)
    (global-set-key (kbd "S-SPC") 'ibus-toggle)
    (global-set-key (kbd "C-x M-i") 'ibus-mode)
    (setq ibus-cursor-color '("red" "white" "limegreen")))

;; geometry for linux
  (if (eq system-type 'gnu/linux)
      (setq default-frame-alist
            '((top . 0) (left . 250)
              (width . 90) (height . 30)))))

;; recentf
(recentf-mode t)
(setq recentf-max-saved-items 30)
(setq recentf-auto-cleanup 300)
(setq recentf-save-file "~/.emacs.d/recentf-list")

;; misc functions
(defun reload-dot-emacs ()
  "Save the .emacs buffer if needed, then reload .emacs."
  (interactive)
  (let ((dot-emacs "~/.emacs"))
    (and (get-file-buffer dot-emacs)
         (save-buffer (get-file-buffer dot-emacs)))
    (load-file dot-emacs))
  (message "Re-initialized!"))

(defun copy-lines (&optional arg)
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (next-line arg)
    (kill-ring-save (mark) (point))))

(global-set-key (kbd "C-c C-w") 'copy-lines)
(global-set-key (kbd "M-d") 'kill-whole-line)
(global-set-key (kbd "C-c C-j") 'join-line)

;; js2-mode
;; emacs -batch -f batch-byte-compile *.el
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
