(set-keyboard-coding-system nil)
(global-linum-mode t)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

; Use-package to replace require
(require 'use-package)

; Download packages if they are missing.
(setq use-package-always-ensure t)

(use-package smartparens-config
	     :ensure smartparens
	     :config
	     (progn
	       (show-smartparens-global-mode t)))

;;(use-package expand-region)
;;(global-set-key (kbd "C-=") 'er/expand-region)

(use-package ido)
(use-package smex)

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-smartparens-strict-mode)
