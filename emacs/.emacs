;;; package --- summary:
;;; Commentary:
;;; Code:
(set-keyboard-coding-system nil)
; (global-linum-mode t)
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
;; (package-refresh-contents)

;; Use-package to replace require
(load-file "~/dotfiles/emacs/get-use-package.el")
(require 'use-package)

; Download packages if they are missing.
(setq use-package-always-ensure t)

(use-package linum-relative)

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

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d%d) ")
  (setq ivy-use-selectable-prompt t))

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))

(use-package ace-window
  :ensure t
  :init
  (setq aw-dispatch-always t))

;; evil
(use-package evil
  :config
  (setq evil-want-visual-char-semi-exclusive t)
  (setq evil-want-fine-undo t)
  (evil-mode 1))

(use-package evil-smartparens)

;; git support
(use-package magit)

(use-package evil-magit)

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
      (embrace-add-pair ?a "\\begin{align*}" "\\end{align*}")
      (embrace-add-pair ?E "\\begin{equation}" "\\end{equation}")
      (embrace-add-pair ?m "\\(" "\\)")
      (embrace-add-pair ?M "\\[" "\\]")
      (embrace-add-pair ?l "\\left(" "\\right)")
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

(use-package evil-matchit)
(global-evil-matchit-mode 1)

(use-package general)

(general-create-definer my-leader-def
  :states '(normal emacs)
  :prefix "SPC")

; avy for jumping to words in file
(use-package avy
 :ensure t
 :config
 (global-set-key (kbd "C-c j") 'avy-goto-char-2))


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

(use-package sage-shell-mode
    :init
    (setq sage-shell:sage-root "~/sage"))

(defun send-to-sage-and-switch ()
    "Send buffer to sage and switch to sage buffer."
    (interactive)
    (progn
    (caill-interactively 'sage-shell-edit:send-buffer)
    (call-interactively 'other-window)))

(defun generate-sage-tags ()
"Generate a tags file for all *.sage files in current directory."
(interactive)
(shell-command "find . -name \"*.sage\" -print | etags -l \"python\" -"))

(my-leader-def 'sage-shell:sage-mode-map
    "c" 'sage-shell-edit:send-buffer)

(general-define-key
 :states '(normal emacs)
 :keymaps '(sage-shell-mode-map)
 "SPC p" 'counsel-shell-history)

;; Get the correct texlive in emacs path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2020/bin/x86_64-linux"))
(setq exec-path (append exec-path '("/usr/local/texlive/2020/bin/x86_64-linux")))

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
    (pdf-tools disable-mouse general python-docstring auctex magit smex smartparens use-package))))
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
(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
    TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
    #'TeX-revert-document-buffer))

(my-leader-def 'pdf-view-mode-map
  "SPC" 'pdf-view-scroll-up-or-next-page)

(general-define-key
 :states '(normal emacs)
 :keymaps '(pdf-view-mode-map)
 "k" 'pdf-view-previous-line-or-previous-page
 "j" 'pdf-view-next-line-or-next-page
 "l" 'image-forward-hscroll
 "h" 'image-backward-hscroll
 "C-f" 'pdf-view-scroll-up-or-next-page
 "C-b" 'pdf-view-scroll-down-or-previous-page)


;; Disable mouse in graphical emacs
(use-package disable-mouse)
(global-disable-mouse-mode)

(mapc #'disable-mouse-in-keymap
      (list evil-motion-state-map
	    evil-normal-state-map
	    evil-visual-state-map
	    evil-insert-state-map))

;; Be able to undo closing a bunch of windows
    (when (fboundp 'winner-mode)
      (winner-mode 1))


(my-leader-def
  "x" 'counsel-M-x
  "b b" 'switch-to-buffer
  "b o" 'switch-to-buffer-other-window
  "b k" 'kill-current-buffer
  "b l" 'linum-relative-mode
  "f s" 'save-buffer
  "f S" 'write-file
  "f f" 'find-file
  "h k" 'describe-key
  "h f" 'describe-function
  "h m" 'describe-mode
  "h v" 'describe-variable
  "j j" 'avy-goto-char
  "j l" 'avy-goto-line
  "w w" 'ace-window
  "w s" 'ace-swap-window
  "w h" 'evil-window-left
  "w l" 'evil-window-right
  "w j" 'evil-window-down
  "w k" 'evil-window-up
  "w 1" 'delete-other-windows
  "w 0" 'delete-window
  "w 2" 'split-window-vertically
  "w 3" 'split-window-horizontally
  "w u" 'winner-undo
  "w r" 'winner-redo)
  
(provide '.emacs)
;;; .emacs ends here
