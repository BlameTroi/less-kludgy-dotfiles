;;; package -- troy's emacs configuration,

;;; Commentary:
;; this init was inspired by many things i've seen exploring,
;; i don't remember where all the ideas came from. certainly
;; susam on github, Alois Janicek on github, and tess o'connor's
;; config, but there are others.


;;; Code:



;; keep directories clean by stashing autosaves and backups
;; elsewhere.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
(setq backup-by-copying t)


;; encoding, utf-8 everywhere
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)


;; general performance tweaks
(setq gc-cons-threshold 100000000)
(add-function
 :after after-focus-change-function
 (lambda () (unless (frame-focus-state) (garbage-collect))))

;; some standard libraries, but i do wish cl-lib would remove
;; that cl deprecated warning.
(require 'cl-lib)     ;; use the right cl libraries
(require 'seq)        ;; sequence operations


;; package management
;; add base melpa and the new nongnu archives. it's not clear which
;; should be preferred, but so far nongnu is pretty small.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             (cons "nongnu" (format "http%s://elpa.nongnu.org/nongnu/"
                                    (if (gnutls-available-p) "s" ""))))
(unless package--initialized
  (package-initialize))
(setq package-selected-packages
      '(
        use-package

        evil
        evil-leader
        evil-easymotion
        evil-ediff
        evil-goggles
        evil-lispy
        evil-mark-replace
        evil-matchit
        evil-nerd-commenter
        evil-org
        evil-paredit
        evil-search-highlight-persist
        evil-surround
        evil-visualstar

        visible-mark
        goto-last-change
        undo-fu

        which-key

        company

        rainbow-delimiters

        flycheck

        yasnippet
        yasnippet-classic-snippets
        common-lisp-snippets
        go-snippets

        fzf
        ag

        treemacs
        treemacs-all-the-icons
        treemacs-evil
        treemacs-icons-dired
        treemacs-magit
        treemacs-persp
        treemacs-perspective
        treemacs-projectile

        helm

        powerline
        powerline-evil

        projectile

        sml-basis
        sml-mode

        exec-path-from-shell

        forth-mode

        markdown-mode

        go-mode

        lua-mode
        luarocks

        eldoc
        c-eldoc

        geiser
        geiser-mit
        geiser-guile

        magit

        org

        paredit
        paredit-everywhere
        paredit-menu

        pyvenv

        theme-magic
        green-is-the-new-black-theme
        green-screen-theme
        alect-themes)
      )
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))


;; basic evil mode. modal editing is the way.
(require 'evil)
(evil-mode 1)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key
  ;; "." 'counsel-find-file
  ;; "," 'counsel-switch-buffer
  ;; "fr" 'counsel-recentf
  "bs" 'save-buffer
  "bk" (lambda ()
         (interactive)
         (kill-buffer (current-buffer)))
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "wc" 'evil-window-delete
  ;; "cs" 'eval-last-sexp
  ;; "q"  'evil-quit
  "he" 'view-echo-area-messages
  ;; "hf" 'counsel-describe-function
  ;; "hv" 'counsel-describe-variable
  "h." 'describe-symbol
  ;; "sb" 'swiper
  ;; "sd" 'counsel-rg
  )


;; company mode
(company-mode)


;; projectile
(projectile-mode +1)
;;(define-key projectile-mode-map (kdb "s-p") 'projectile-command-map)
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; theming and ui
;; (load-theme 'alect-black-alt t)
;; (load-theme 'green-screen t)
(load-theme 'wombat)
(set-face-background 'default "#111")
(set-face-background 'cursor "#c96")
(set-face-background 'isearch "#c60")
(set-face-foreground 'isearch "#eee")
(set-face-background 'lazy-highlight "#960")
(set-face-foreground 'lazy-highlight "#ccc")
(set-face-foreground 'font-lock-comment-face "#fc0")


;; i need bigger fonts
(set-frame-font "Hack Nerd Font Mono-15.0" nil t)


;; a better mode line
(require 'powerline)
(require 'powerline-evil)
(powerline-default-theme)


;; my preferences, avoid but don't completely hide the
;; gui aspects
(column-number-mode)
(setq-default visible-bell t)
(setq inhibit-startup-screen t)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq-default
 fill-column 70            ; width for auto line wrap
 indent-tabs-mode nil      ; space it out
 initial-scratch-message nil ; the abyss isn't quite so empty
 select-enable-clipboard t ; this should integrate kill ring with system
 )
(display-time-mode 1)    ; display clock
(fset 'yes-or-no-p 'y-or-n-p)         ; less typing
(setq read-file-name-completion-ignore-case t)
(recentf-mode)

;; which key, what key, why key?
(require 'which-key)
(which-key-mode)


;; whitespace and tabs. some modes (go, make, what else) want tabs
;; for indents, but my default is spaces.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
(setq sentence-end-double-space nil)


;; parens should be colorful and show matches
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray
(setq show-paren-delay 0)
(show-paren-mode)


;; ace-window for better window navigation
(use-package ace-window :ensure t)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))


;; syntax checking
(global-flycheck-mode)

;; snippets
(require 'yasnippet)
(yas-global-mode 1)


;; for whenever i start using emacsclient
;; define function to shutdown emacs server instance
;; (defun server-shutdown ()
;;   "Save buffers, Quit, and Shutdown (kill) server"
;;   (interactive)
;;   (save-some-buffers)
;;   (kill-emacs)
;; )


;; get the various custom-set-variable blocks out
;; of my init.el
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)

  (load custom-file))


;; this is time sensitive during init


(provide 'init)

;;; init.el ends here
