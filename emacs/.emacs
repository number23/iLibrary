(global-set-key (kbd "C-SPC") 'nil)
(setq default-directory "~/")
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq gnus-inhibit-startup-message t)

(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28))
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(scroll-bar-mode 0)

(when window-system
  (if (eq system-type 'darwin)
      (progn
        (tool-bar-mode 0)
        (setq epg-gpg-program "/usr/local/bin/gpg")
        (setq ns-pop-up-frames nil)
        ;;(set-frame-font "Monaco:style=Roman:size=16")
        (set-frame-font "YaHei Consolas Hybrid:size=16")))

  (if (eq system-type 'gnu/linux)
      (progn
        (tool-bar-mode 0)
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
(setq kill-ring-max 500)
(setq-default require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-to-list 'load-path' "~/.emacs.d/site-lisp")
(global-auto-revert-mode t)
(setq vc-follow-symlinks t)
(setq make-backup-files nil)

(when window-system
  (load "server")
  (unless (server-running-p) (server-start)))

(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/github.com/replace-colorthemes"))

(load-theme 'gnome2 t t)
(enable-theme 'gnome2)

;;; recentf
(recentf-mode t)
(setq recentf-max-saved-items 30)
(setq recentf-max-menu-items 25)
(setq recentf-auto-cleanup 300)
(setq recentf-save-file "~/.emacs.d/recentf-list")

;;; EasyPG
(require 'epa-file)
(epa-file-enable)

;;; package.el
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(use-package
                      lsp-mode
                      paredit
                      highlight-parentheses
                      clojure-mode
                      markdown-mode
                      auto-complete))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(require 'highlight-parentheses)
(dolist (mode '(clojure clojurescript python scheme emacs-lisp lisp))
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

;;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (append
       '(("\\.md$"  . markdown-mode)
         ("\\.markdown$"  . markdown-mode))
       auto-mode-alist))

;;; auto-complete: M-n, M-p, C-g
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20201213.1255/dict/")
(ac-config-default)

;;; misc functions
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

(load-library "hideshow")
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-iso-lefttab] 'previous-buffer)
(global-set-key (kbd "C-c C-j") 'join-line)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key [f9] 'toggle-hiding)
(global-set-key [f10] 'toggle-selective-display)

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

(global-set-key [f11] 'switch-buffer-scratch)
(global-set-key (kbd "C-%") 'goto-match-parenthesis)

(defvar swap-paren-pairs '("()" "[]"))
(defun swap-parens-at-points (b e)
  (let ((open-char (buffer-substring b (+ b 1)))
        (paren-pair-list (append swap-paren-pairs swap-paren-pairs)))
    (while paren-pair-list
      (if (eq (aref open-char 0) (aref (car paren-pair-list) 0))
          (save-excursion
            (setq to-replace (cadr paren-pair-list))
            (goto-char b)
            (delete-char 1)
            (insert (aref to-replace 0))
            (goto-char (- e 1))
            (delete-char 1)
            (insert (aref to-replace 1))
            (setq paren-pair-list nil))
        (setq paren-pair-list (cdr paren-pair-list))))))

(defun swap-parens ()
  (interactive)
  (cond ((looking-at "\\s(")
         (swap-parens-at-points (point) (save-excursion (forward-sexp) (point))))
        ((and (> (point) 1) (save-excursion (forward-char -1) (looking-at "\\s)")))
         (swap-parens-at-points (save-excursion (forward-sexp -1) (point)) (point)))
        ((message "Not at a paren"))))
(global-set-key [f12] 'swap-parens)

;;; golang
;; go get golang.org/x/tools/gopls@latest

;; M-. 和 M-, 定义跳转/跳回
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))
