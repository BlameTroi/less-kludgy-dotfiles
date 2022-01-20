;; modal editing, or more correctly, movement, using a
;; minor mode and modeled after vi.
;;
;; originally from:
;; https://llazarek.github.io/2018/07/modal-editing-in-emacs.html
;;
;; but of course i'll extend and tweak it.
;;
;; blametroi is troy brumley, january 2022

(provide 'troi-modal)

(define-minor-mode troi-command-mode
  "troi-command-mode is a minor mode for modal editing.

Use `toggle-troi-command-mode' to enter and exit the mode.

troi-command-mode defines the following bindings:
\\{troi-command-mode-map}
"
  ;; initial value
  nil
  ;; indicator for mode line
  " modal"
  ;; minor mode bindings
  '(((kbd "i") . toggle-troi-command-mode)

    ((kbd "j") . next-line)
    ((kbd "k") . previous-line)
    ((kbd "h") . backward-char)
    ((kbd "l") . forward-char))
  :group 'troi-group)

;; todo: page, top, bottom, split, scroll

(defun toggle-troi-command-mode (&optional set-state)
  "Toggle `troi-command-mode', optionally ensuring its state with `SET-STATE'.

`SET-STATE' is interpreted as follows:
  nil   (Same as no argument) Toggle `troi-command-mode'
  -1    Ensure `troi-command-mode' is disabled
  else  Ensure `troi-command-mode' is enabled
"
  (interactive)
  (cond ((equal set-state -1)
         (when troi-command-mode
           (troi-command-mode -1)))

        ((equal set-state nil)
         (troi-command-mode (if troi-command-mode -1 1)))

        (t
         (unless troi-command-mode
           (troi-command-mode 1)))))

(global-set-key (kbd "M-SPC") 'toggle-troi-command-mode)
