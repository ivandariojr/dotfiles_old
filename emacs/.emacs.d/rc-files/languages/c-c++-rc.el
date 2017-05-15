;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; use 4 spaces by default, select formatting style
(add-hook 'c-mode-common-hook 
          (lambda ()
            (c-set-style "bsd") ; I've written too much java
            (setq c-basic-offset 4)
            (setq comment-start "// ")
            (setq comment-end "")))

(require 'cedet)
(require 'semantic)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(semantic-mode 1)

(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)
(setq semanticdb-default-submodes '(global-semantic-idle-scheduler-mode
                                    global-semanticdb-minor-mode))

(defun my-add-cedet-bindings ()
  (local-set-key (kbd "C-<return>") 'semantic-ia-complete-symbol)
  (local-set-key (kbd ">") 'semantic-complete-self-insert)
  (local-set-key (kbd ".") 'semantic-complete-self-insert)
  (local-set-key (kbd "C-c p") 'semantic-ia-show-summary)
  (local-set-key (kbd "M-.") 'semantic-ia-fast-jump))

(defun my:add-semantic-to-autocomplete ()
  (add-to-list 'ac-sources 'ac-source-semantic)
)

(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
(add-hook 'c-mode-common-hook 'my-add-cedet-bindings)
(add-hook 'c++-mode-common-hook 'my-add-cedet-bindings)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C++ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push '("\\.ino$" . c++-mode)  auto-mode-alist)

