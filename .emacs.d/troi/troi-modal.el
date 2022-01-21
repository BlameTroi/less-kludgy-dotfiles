;; modal editing, or more correctly, movement, using a
;; minor mode and modeled after vi.
;;
;; originally from:
;; https://llazarek.github.io/2018/07/modal-editing-in-emacs.html
;;
;; but of course i'll extend and tweak it.
;;
;; blametroi is troy brumley, january 2022
;;
;; to do items:
;;
;; always switch to modal when opening a new buffer
;; vim style D, P, O

(provide 'troi-modal)

(define-minor-mode troi/command-mode
  "troi-command-mode is a minor mode for modal editing supporting a subset of the vi command mode.

Use `toggle-troi/command-mode' to enter and exit the mode.

troi/command-mode defines the following bindings:
\\{troi/command-mode-map}
"
  ;; initial value
  nil
  ;; indicator for mode line
  " modal"
  ;; minor mode bindings
  '(((kbd "i") . toggle-troi/command-mode)

    ((kbd "h") . backward-char)
    ((kbd "j") . next-line)
    ((kbd "k") . previous-line)
    ((kbd "l") . forward-char)

    ((kbd "w") . right-word)
    ((kbd "b") . left-word)

    ((kbd "^") . move-beginning-of-line)
    ((kbd "$") . move-end-of-line)

    ((kbd "C-f") . scroll-up-command)
    ((kbd "C-b") . scroll-down-command)
    ((kbd "Z") . recenter-top-bottom)

    ((kbd ":") . goto-line))

  :group 'troi/group)

(defun toggle-troi/command-mode (&optional set-state)
  "Toggle `troi/command-mode', optionally ensuring its state with `SET-STATE'.

`SET-STATE' is interpreted as follows:
  nil   (Same as no argument) Toggle `troi/command-mode'
  -1    Ensure `troi/command-mode' is disabled
  else  Ensure `troi/command-mode' is enabled
"
  (interactive)
  (cond ((equal set-state -1)
         (when troi/command-mode
           (troi/command-mode -1)))

        ((equal set-state nil)
         (troi/command-mode (if troi/command-mode -1 1)))

        (t
         (unless troi/command-mode
           (troi/command-mode 1)))))

(global-set-key (kbd "M-SPC") 'toggle-troi/command-mode)
