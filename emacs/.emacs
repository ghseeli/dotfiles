;;; package --- summary:
;;; Commentary:
;;; Code:
(set-keyboard-coding-system nil)
(global-linum-mode t)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Use-package to replace require
(load-file "~/dotfiles/emacs/get-use-package.el")
(require 'use-package)

; Download packages if they are missing.
(setq use-package-always-ensure t)

(use-package smartparens-config
	     :ensure smartparens
	     :config
	     (progn
	       (show-smartparens-global-mode t)))
(smartparens-global-mode 1)
(sp-local-pair '(LaTeX-mode) "'" "'" :actions nil)
;;(use-package expand-region)
;;(global-set-key (kbd "C-=") 'er/expand-region)

;; general productivity
(use-package ido)
(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command))
  )

; avy for jumping to words in file
(use-package avy
 :ensure t
 :config
 (global-set-key (kbd "C-c j") 'avy-goto-char-2))

;; git support
(use-package magit)

;; ispell
(use-package ispell)
(provide 'setup-spell)
(setq ispell-dictionary "english")

; flycheck to check things
 (use-package flycheck
  :init (global-flycheck-mode))
(setq-default flycheck-disabled-checkers '(tex-lacheck)) ; disabled because it is slowing down big files.


; yasnippet for better LaTeX macro-ing
(add-to-list 'load-path
	     "~/.emacs/yasnippet")
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

;; LaTeX ; eventually should be moved to seperate file.
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;; (add-hook 'LaTeX-mode-hook 'turn-on-smartparens-strict-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-remote-arguments nil)
 '(magit-subtree-arguments nil)
 '(package-selected-packages (quote (auctex magit smex smartparens use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Compile LaTeX with latexmk and put outputs into ./out folder.
(add-hook 'LaTeX-mode-hook (lambda ()
                 (push
                  '("Make" "latexmk -pdf -interaction=nonstopmode -pv -outdir=./out %t" TeX-run-TeX nil t
                :help "Make pdf output using latexmk.")
                  TeX-command-list)))
(add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "Make")))
(provide '.emacs)
;;; .emacs ends here
