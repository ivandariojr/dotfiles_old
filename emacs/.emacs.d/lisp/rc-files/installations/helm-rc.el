;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-helm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; improves buffer-switching, autocomplete, and a lot of other things.

(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c C-h") 'helm-mini)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
;;; Setting helm-tex path environment variable make sure to change it after
(setq helm-bibtex-bibliography "/home/ivan/Documents/Latex_Stuff/BibDB/AllEntries.bib")
(setq helm-bibtex-library-path "/home/ivan/Documents/Papers")
