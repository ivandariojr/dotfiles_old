;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; paredit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; handle parentheses in lispy languages better
(when (require 'paredit nil 'noerror)
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'slime-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'lisp-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'scheme-mode-hook (lambda () (paredit-mode 1))))


;; Stop SLIME's REPL from grabbing DEL, which is annoying when
;; backspacing over a '('
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)))
