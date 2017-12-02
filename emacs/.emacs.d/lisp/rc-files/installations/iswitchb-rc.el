;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iswitchb ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; improves buffer-switching
(iswitchb-mode)
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (kbd key) fun)))
        '(("C-b" . iswitchb-prev-match)
          ("C-f" . iswitchb-next-match))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
(setq iswitchb-buffer-ignore '(" *Minibuf-1*" "*elpy-rpc*"))
