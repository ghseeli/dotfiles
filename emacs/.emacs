;;; package --- summary:
;;; Commentary:
;;; Code:
(set-keyboard-coding-system nil)
; (global-linum-mode t)
(setq column-number-mode t)

(setq inhibit-startup-screen t)
(setq package-check-signature nil)
(setq backup-directory-alist `(("." . "~/.emacs_backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
(setq vc-make-backup-files t)
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

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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

(use-package gscholar-bibtex)

(use-package ivy-bibtex
   :config
   (setq ivy-re-builders-alist
       '((ivy-bibtex . ivy--regex-ignore-order)
	 (t . ivy--regex-plus))))

 (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

;; evil
(use-package evil
  :config
  (setq evil-want-visual-char-semi-exclusive t)
  (setq evil-want-fine-undo t)
  (evil-mode 1))

(use-package evil-smartparens)

(use-package evil-org
    :ensure t
    :after org
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    ;; (add-hook 'evil-org-mode-hook
    ;; 	    (lambda ()
    ;; 	    (evil-org-set-key-theme)))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

(evil-set-initial-state 'org-agenda-mode 'emacs)

;; Grammar-check
(use-package langtool
  :config
  (setq langtool-java-bin "/usr/bin/java")
  (setq langtool-language-tool-jar "~/LanguageTool/languagetool-commandline.jar"))

(use-package langtool-ignore-fonts
  :load-path "~/.emacs.d/elpa/")

;; git support
(use-package magit)

(use-package evil-magit)

(use-package diff-hl)
(global-diff-hl-mode)

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-replace-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-visual-state-map  "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map  "jj" "\\")
)


(use-package paredit)

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
      (embrace-add-pair ?q "``" "''" )
      (defun embrace-with-command ()
	(let ((fname (read-string "Command: ")))
	  (cons (format "\\%s{" (or fname "")) "}")))
      (embrace-add-pair-regexp ?c "\\(\\w\\|\\s_\\)+?(" ")" 'embrace-with-command
                           (embrace-build-help "\command{" "}")))))


(use-package evil-embrace
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-matchit)
(global-evil-matchit-mode 1)

(use-package general)

(general-create-definer my-leader-def
  :states '(normal emacs)
  :keymaps '(global magit-mode-map)
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
(flycheck-define-checker textlint
  "A linter for textlint."
  :command ("npx" "textlint"
            "--config" "/home/rob/.emacs.d/.textlintrc"
            "--format" "unix"
            "--rule" "write-good"
            "--rule" "no-start-duplicated-conjunction"
            "--rule" "max-comma"
            "--rule" "terminology"
            "--rule" "period-in-list-item"
            "--rule" "abbr-within-parentheses"
            "--rule" "alex"
            "--rule" "common-misspellings"
            "--rule" "en-max-word-count"
            "--rule" "diacritics"
            "--rule" "stop-words"
            "--plugin"
            (eval
             (if (derived-mode-p 'tex-mode)
                 "latex"
               "@textlint/text"))
            source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode latex-mode org-mode markdown-mode)
  )
(add-to-list 'flycheck-checkers 'textlint)

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

(use-package skeletor
  :config
  (skeletor-define-template "coverletter-skeleton"
    :title "coverletter-skeleton"
    :no-license? t)
  (skeletor-define-template "latex-skeleton"
    :title "latex-skeleton"
    :no-license? t)
  (skeletor-define-template "worksheet-skeleton"
    :title "worksheet-skeleton"
    :substitutions
    '(("__WORKSHEETNUM__" . (lambda () (read-string "Worksheet Number: "))) ("__SECTIONNUM__" . (lambda () (read-string "Section Number: "))))
    ;; '(("__DESCRIPTION__" . (lambda () (read-string "Description: "))))
    ;; :substitutions
    ;; 
    ;; :substitutions
    :no-license? t)
  )

(use-package python-docstring)

(use-package sage-shell-mode
    :init
    (setq sage-shell:sage-executable "~/sage"))

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

;; Gap-mode
(use-package gap-mode
    :init
    (setq gap-executable "/Applications/gap/bin/gap.sh"))

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
   '(evil-org pdf-tools disable-mouse general python-docstring auctex magit smex smartparens use-package))
 '(sage-shell:use-prompt-toolkit nil)
 '(sage-shell:use-simple-prompt t))
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
;; test getting to compile on arm Mac
;; (setenv "CPATH" "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include")
;; (setenv "PKG_CONFIG_PATH" "/opt/homebrew/Library/Homebrew/os/mac/pkgconfig/11.1")
(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
    TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
    #'TeX-revert-document-buffer))

;; To help with scaled displays
(setq pdf-view-use-scaling t)

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
 "C-b" 'pdf-view-scroll-down-or-previous-page
 "gg" 'pdf-view-first-page
 "G" 'pdf-view-last-page
 "r" 'revert-buffer
 ":" 'evil-ex)


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
  "w r" 'winner-redo
  "o a" 'org-agenda
  "o c c" 'org-capture
  "o c i" 'org-clock-in
  "o c o" 'org-clock-out
  )


;; Borrowed from Chris Lloyd to search current LaTeX project for all the labels and then insert one using the completion framework. Based on the reftex function goto-label.

(defun my-ref-label (&optional other-window)
  "Prompt for a label (with completion) and insert a reference to it."
  (interactive "P")
  (reftex-access-scan-info)
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
	 ;; If point is inside a \ref{} or \pageref{}, use that as
	 ;; default value.
	 (default (when (looking-back "\\\\\\(?:page\\)?ref{[-a-zA-Z0-9_*.:]*"
                                      (line-beginning-position))
		    (reftex-this-word "-a-zA-Z0-9_*.:")))
         (label (completing-read (if default
				     (format "Label (default %s): " default)
				   "Label: ")
				 docstruct
                                 (lambda (x) (stringp (car x))) t nil nil
				 default)))
    (insert (concat "\\ref{" label "}"))))
  
(my-leader-def 'LaTeX-mode-map
  "r r" 'ivy-bibtex-with-local-bibliography
  "r l" 'my-label-ref
  )

;; org-mode
(setq org-directory "~/Documents/org")
(setq org-agenda-files '("~/Documents/org"))
(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-default-notes-file (concat org-directory "/refile.org"))

;; Capture templates for: TODO tasks, Notes, phone calls, meetings
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Documents/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/Documents/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/Documents/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/Documents/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
	     )))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


(provide '.emacs)
;;; .emacs ends here
