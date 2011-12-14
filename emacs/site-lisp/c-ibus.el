;; ibus and geometry for linux X window
(when (and (eq window-system 'x)
           (eq system-type 'gnu/linux))
  (require 'ibus)
  (add-hook 'after-init-hook 'ibus-mode-on)
  (ibus-define-common-key ?\C-\s nil)
  (ibus-define-common-key ?\C-/ nil)
  (ibus-define-common-key ?\S-\s nil)
  (global-set-key (kbd "S-SPC") 'ibus-toggle)
  (global-set-key (kbd "C-x M-i") 'ibus-mode)
  (setq ibus-cursor-color '("red" "white" "limegreen"))
  (setq default-frame-alist
        '((top . 0) (left . 250)
          (width . 90) (height . 30))))

(provide 'c-ibus)
