;;; cl-rc.el --- summary
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(push '("\\.sbclrc$" . lisp-mode)  auto-mode-alist )
(push '("\\.asdf-install$" . lisp-mode) auto-mode-alist )
(push '("\\.asd$" . lisp-mode) auto-mode-alist )


(require 'pretty-lambdada)
(pretty-lambda-for-modes)

(require 'pretty-mode )
(global-pretty-mode t)


(require 'slime)
(require 'slime-autoloads)
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/clisp")
;; (setq inferior-lisp-program "sbcl --noinform --no-linedit")



(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-asdf
                    slime-autodoc
                    slime-editing-commands
                    slime-fancy
                    slime-fontifying-fu
                    slime-fuzzy
                    slime-indentation
                    slime-mdot-fu
                    slime-package-fu
                    slime-references
                    slime-repl
                    slime-sbcl-exts
                    slime-scratch
                    slime-xref-browser
                    ))))

(slime-setup)

(add-hook 'slime-mode-hook
          (lambda ()
            (local-set-key "C-c C-c" slime-eval-buffer)
            (local-set-key "C-c d" slime-eval-buffer)
            (local-set-key "C-c C-d" slime-eval-defun)
            (local-set-key "C-c r" slime-eval-region)
            (local-set-key "C-c C-r" slime-eval-region)))

;; (add-to-list 'load-path "/var/lib/chicken/6/")   ; Where Eggs are installed
;; (autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
;; (setq slime-csi-path "/usr/bin/csi")

(add-hook 'scheme-mode-hook
          (lambda ()
            (slime-mode t)))

(add-hook 'scheme-mode-hook 'turn-on-pretty-lambda-mode)
(add-hook 'slime-mode-hook 'turn-on-pretty-lambda-mode)

;;; cl-rc.el ends here
