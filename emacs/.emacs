;;; package --- summary:
;;; Commentary:
;;; Code:
(set-keyboard-coding-system nil)
(global-linum-mode t)
(setq column-number-mode t)

(setq inhibit-startup-screen t)
(setq package-check-signature nil)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)
(package-refresh-contents)

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

;; evil
(use-package evil
  :config
  (setq evil-want-visual-char-semi-exclusive t)
  (setq evil-want-fine-undo t)
  (evil-mode 1))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-replace-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-visual-state-map  "jk" 'evil-normal-state))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (evil-add-to-alist
   'evil-surround-pairs-alist
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ?\) '("( " . " )")
   ?\] '("[ " . " ]")
   ?\} '("{ " . " }")))

(use-package paredit)

(use-package embrace
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook
    (lambda ()
      (embrace-add-pair ?e "\\begin{" "}")
      (embrace-add-pair ?m "\\(" "\\)")
      (embrace-add-pair ?M "\\[" "\\]")
      (defun embrace-with-command ()
	(let ((fname (read-string "Command: ")))
	  (cons (format "\\%s{" (or fname "")) "}")))
      (embrace-add-pair-regexp ?c "\\(\\w\\|\\s_\\)+?(" ")" 'embrace-with-command
                           (embrace-build-help "\command{" "}")))))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
(evil-add-to-alist
 'evil-surround-pairs-alist
 ?\( '("(" . ")")
 ?\[ '("[" . "]")
 ?\{ '("{" . "}")
 ?\) '("( " . " )")
 ?\] '("[ " . " ]")
 ?\} '("{ " . " }")))

(use-package evil-embrace
  :config
  (evil-embrace-enable-evil-surround-integration))

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

;; color themes
;; (use-package sublime-themes
;;  :init (progn (load-theme 'spolsky t)))


; yasnippet for better LaTeX macro-ing
(add-to-list 'load-path
	     "~/.emacs/yasnippet")
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package python-docstring)

(use-package tex
  :ensure auctex
  :init
;  (add-hook 'LaTeX-mode-hook (lambda () (flycheck-select-checker 'tex-chktex)))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (push
				'("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
				  :help "Run Latexmk on file")
				TeX-command-list)))
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (add-hook 'LaTeX-mode-hook 'server-start);

  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
)

;; LaTeX ; eventually should be moved to seperate file.
;;(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;; (add-hook 'LaTeX-mode-hook 'turn-on-smartparens-strict-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-remote-arguments nil)
 '(magit-subtree-arguments nil)
 '(package-selected-packages
   (quote
    (general python-docstring auctex magit smex smartparens use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

 ;; Compile LaTeX with latexmk and put outputs into ./out folder.
;; (add-hook 'LaTeX-mode-hook (lambda ()
;;                  (push
;;                   '("Make" "latexmk -pdf -interaction=nonstopmode -pv -outdir=./out %t" TeX-run-TeX nil t
;;                 :help "Make pdf output using latexmk.")
;;                   TeX-command-list)))
;; (add-hook 'LaTeX-mode-hook '(lambda () (setq TeX-command-default "Make")))
(add-hook 'LaTeX-mode-hook (lambda () (setq font-lock-maximum-decoration 200)))

(use-package general)

(general-create-definer my-leader-def
  :states '(normal)
  :prefix "SPC")

(my-leader-def
  "x" 'counsel-M-x
  "b b" 'switch-to-buffer
  "b o" 'switch-to-buffer-other-window
  "b k" 'kill-current-buffer
  "j j" 'avy-goto-char
  "j l" 'avy-goto-line
  "f s" 'save-buffer
  "f S" 'write-file
  "h k" 'describe-key
  "h f" 'describe-function
  "h m" 'describe-mode
  "h v" 'describe-variable)
  
(provide '.emacs)
;;; .emacs ends here
