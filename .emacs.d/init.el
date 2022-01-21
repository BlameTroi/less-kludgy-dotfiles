;; troi's perpetual quest for a better init.el
;;
;; stripping down and building up from scratch
;;
;; various articles and videos have influenced
;; this effort, but as i've learned more and
;; stopped using some of the usual starter kits
;; i find the following useful:
;;
;; http://github.com/susam/emacs4cl
;; http://github.com/susam/emfy
;; http://tuhdo.github.io
;; http://emacswiki.org
;; http://tess.oconnor.cx/config/.emacs
;;
;; tess o'connor's config is particularly well
;; laid out and documented.

;; performance tweaks
(add-hook 'focus-out-hook 'garbage-collect)
(setq gc-cons-threshhold 1000000000)


;; my code, overrides, and such
(add-to-list 'load-path "~/.emacs.d/troi")
(require 'troi-modal)


;; encoding, utf-8 everywhere
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)


;; whitespace control
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq sentence-end-double-space nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;; clean, quite, minimal, usable
;;(menu-bar-mode 0)
;;(when (display-graphic-p)
;;  (tool-bar-mode 0)
;;  (scroll-bar-mode 0))
(column-number-mode)
(setq inhibit-startup-screen t)
(setq-default
 visible-bell t
 select-enable-clipboard t)
(setq read-filename-completion-ignore-case t)
(setq read-buffer-name-completion-ignore-case t)
(xterm-mouse-mode)
(fset 'yes-or-no-p 'y-or-n-p)         ; less typing

;; theme
;; currently using an old color theme until i figure
;; out which custom theme i want and how to set it
;; up.
;;
;; themes i find readable after some experimentation:
;;
;; alect-black, alect-black-alt, distinquished, grandshell,
;; green-is-the-new-black, green-phosphor, green-screen,
;; gruber-darker, hemisu-dark, ir-black, seti,
;; base16-greenscreen, base16-irblack, base16-isotope,
;; base16-seti, srcery, tron-legacy, deeper-blue,
;; manoj-dark, wheatgrass
;; (load-theme 'wheatgrass)


;; global default fonts
(add-to-list 'default-frame-alist '(font . "Hack Nerd Font Mono-16"))
(set-frame-font "Hack Nerd Font Mono-16" nil t)


;; interactively do things
(ido-mode 1)
(ido-everywhere)
(setq ido-enable-flex-matching t)
(fido-mode)


;; keep auto saves and backups out of sight
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))


;; do not move current file while creating backup
(setq backup-by-copying t)


;; paren matching when lisping
(setq show-paren-delay 0)
(show-paren-mode)


;; customizations in a separate file
;; *** this must come before any package-install ***
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)


;; package management & melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(unless package-archive-contents
  (package-refresh-contents))
(package-initialize)


;; make sure path is updated with shell path
;; when launched from a gui shell it isn't.
;;
;; TODO: dedup path
(defun troi/set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL -c 'echo $PATH'"
                                          ))))
       (setenv "PATH" path-from-shell)
       (setq exec-path (split-string path-from-shell path-separator))))
(troi/set-exec-path-from-shell-PATH)


;; install lisp development and other packages
;; here if they are not already installed
(dolist (package
         '(
           slime clhs rainbow-delimiters
           which-key
           go-mode forth-mode))
  (unless (package-installed-p package)
    (package-install package)))


;; inferior lisp when sliming
(setq inferior-lisp-program "sbcl")


;; hooks for lisp with paredit and rainbow, there are
;; many and the line by line explanation at susam/emacs4cl
;; explains them well.


;; rainbow delimiters
(add-hook 'emacs-lisp-mode-hook
          'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook
          'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook
          'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook
          'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook
          'rainbow-delimiters-mode)


;; and color them
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


;; help with keys
(require 'which-key)
(which-key-mode)


;; org mode
(require 'org)
(setq org-log-done t)
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))


;; cl, sbcl, and slime
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")


;; fortran
(autoload 'f90-mode "f90" "Fortran 90 mode" t)
(add-hook 'f90-mode-hook 'troi/f90-mode-hook)
(defun troi/f90-mode-hook ()
  (setq f90-font-lock-keywords f90-font-lock-keywords-3)
  (abbrev-mode 1)                       ; turn on abbreviation mode
  (turn-on-font-lock)                   ; syntax highlighting
  (auto-fill-mode 0))                   ; turn off auto-filling


;; key binds that don't belong anywhere else
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; more to come
