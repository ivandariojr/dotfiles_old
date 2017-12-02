
;;Always use Tex input method
(setq make-backup-files nil)            ;no backups to clutter things up
(setq inhibit-startup-message t)        ;don't put up the splash page

;;; use utf-8 everywhere
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;;; put temporary files somewhere else
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;;; save some stuff across sessions, most importantly the command
;;; history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/tmp/savehist")

;;; save file-open history across sessions with slightly nicer
;;; behavior
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tabs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; global settings for tabs - use spaces!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; but use smart tabs for c and c++
(smart-tabs-insinuate 'c 'c++)
