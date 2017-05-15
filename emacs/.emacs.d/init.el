;;; init --- summary
;;; Commentary:
;;; code:

(setq debug-on-error t)
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "/usr/share/emacs/24.3/lisp/gnus")

;;; require common lisp
(require 'cl)

;;; First, load up some packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)                    ;load everything

;;; install any packages we need
;; ascope is gone
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 '(ac-math 
   ac-slime
   alert
   auctex 
   auto-complete 
   auto-complete-auctex 
   auto-complete-pcmp
   find-file-in-project
   ac-ispell
   bibretrieve
   deferred
   f
   git-rebase-mode
   ebib
   ecb
   git-commit-mode 
   dash 
   flycheck
   desktop-registry
   flycheck-tip
   fuzzy
   find-file-in-project
   helm
   gnome-calendar
   gntp 
   helm
   helm-bibtex
   helm-flycheck
   helm-git
   highlight-indentation
   idomenu 
   kpm-list
   latex-extra
   latex-preview-pane 
   log4e
   lua-mode
   magit
   notify
   org
   org-ac
   org-context
   org-email
   org-gcal
   org-gnome
   org-grep
   org-magit
   org-present 
   org-repo-todo
   pkg-info
   popup 
   pretty-lambdada
   pretty-mode 
   pretty-mode-plus
   request
   request-deferred s
   slime
   telepathy
   w3m
   yaxception
   kpm-list
   nose
   paredit
   smart-tabs-mode
   solarized-theme 
   undo-tree
   virtualenv 
   w3m
   yasnippet
   dired+
   ecb
   cedet
   magic-latex-buffer))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;
;;;  README                                                                 ;;
;;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; things required to use this .emacs:
;;; 
;;; * install from package manager:
;;;   - emacs24
;;;     - ubuntu: ppa:cassou/emacs and emacs-snapshot
;;;   - cscope
;;;   - sbcl
;;;   - cl-swank
;;;   - w3m
;;;   - wl-beta
;;;   - gnutls-bin
;;; * install using package-list-packages:
;;;   - ascope
;;;   - AucTex
;;;   - auto-complete-auctex
;;;   - ac-math
;;;   - auto-complete
;;;   - desktop-registry
;;;   - elpy
;;;   - find-file-in-project
;;;   - helm
;;;   - helm-bibtex
;;;   - highlight-indentation
;;;   - idomenu
;;;   - kpm-list
;;;   - minimap
;;;   - nose
;;;   - paredit
;;;   - smart-tabs-mode
;;;   - solarized-theme
;;;   - undo-tree
;;;   - virtualenv
;;;   - w3m
;;;   - yasnippet
;;; * install to src/ using git:
;;; * set up in other ways
;;;   - pip install --user elpy rope pyflakes pep8 virtualenvwrapper
;;;   - install swank and slime using quicklisp:
;;;       cd ~n
;;;       wget http://beta.quicklisp.org/quicklisp.lisp -O /tmp/quicklisp.lisp
;;;       sbcl --load /tmp/quicklisp.lisp --eval '(quicklisp-quickstart:install :path ".quicklisp/")' --eval '(ql:quickload "quicklisp-slime-helper")' --eval '(ql:quickload "swank")' --eval '(ql:add-to-init-file)' --eval '(quit)'
;;; 
;;; other things that I like to have:
;;; * install from package manager:
;;; * install using package-list-packages:
;;;   - magit
;;; * install to src/ using git:
;;; * set up in other ways


(load "rc-files/configuration-rc.el")
(load "rc-files/installations-rc.el")
(load "rc-files/languages-rc.el")
(load "rc-files/pragmatapro-font-lock-symbols.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom stuff starts here. DO NOT TOUCH ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(pdf-latex-command "pdflatex")
 '(semantic-idle-truncate-long-summaries nil)
 '(slime-truncate-lines nil)
 '(truncate-partial-width-windows nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  CUSTOM                                                                 ;;
;;  do not edit manually!                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "fsdf" :family "PragmataPro")))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;
;;;  COMMANDS TO EXECUTE AT RUNTIME                                         ;;
;;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; enabled commands
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;; init.el ends here
