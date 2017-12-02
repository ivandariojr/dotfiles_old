;;; better compiling - make it easy to reuse the last line without
;;; having to renavigate to the build directory
(global-set-key (kbd "C-c k") 'compile)
(global-set-key (kbd "C-c C-k") 'recompile)

;;; We're using emacsclient exclusively, so we want the user to be
;;; slightly more aware of what's happening - use <C-c #>, <C-x 5 0>,
;;; or 'save-buffers-kill-emacs' as appropriate.
(global-unset-key (kbd "C-x C-c"))

;;; we want to regexp-search by default because regexp search is
;;; awesome.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;; bind recentf
(global-set-key (kbd "C-x f") 'recentf-open-files)

;;; properly handle control-arrows in the terminal
(define-key input-decode-map (kbd "M-[ A") [C-up])
(define-key input-decode-map (kbd "M-[ B") [C-down])
(define-key input-decode-map (kbd "M-[ C") [C-right])
(define-key input-decode-map (kbd "M-[ D") [C-left])

;;; define slightly more intuitive window switching
(global-set-key (kbd "C-x \\") 'split-window-right)
(global-set-key (kbd "C-x C-\\") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)
(global-set-key (kbd "C-x C--") 'split-window-below)

;; Fix iedit bug in Mac
(define-key global-map (kbd "C-c ;") 'iedit-mode)
