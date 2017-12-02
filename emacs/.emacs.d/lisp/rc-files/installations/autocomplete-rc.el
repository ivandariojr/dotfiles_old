;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autocomplete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "M-/") 'hippie-expand)
;; (global-set-key (kbd "M-/") 'company-complete)

;; dirty fix for having AC everywhere
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)


(require 'ac-math) 
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

(eval-after-load "auto-complete"
  '(progn 
     (ac-ispell-setup)))

(defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
   (setq ac-sources
         (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                 ac-sources))
   )

(global-auto-complete-mode t)
 
(setq ac-math-unicode-in-math-p t)

(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)
