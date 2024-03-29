;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;
;;;  ORG-MODE CONFIGURATIONS                                                ;;
;;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; add some extra todo keywords
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "WILL-NOT-DO")
        (sequence "IDEA" "WORKING" "|" "TOO-HARD" "FEASIBLE" "FINISHED")
        (sequence "RECOMMENDED" "ON-HOLD" "|" "READ")))

;;; this has to happen early, so we do it manually instead of having
;;; customize do it
(setq org-mobile-directory "~/.mobileorg")

;;; load org-mode for .org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;;; useful hotkeys
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-a") 'org-agenda)
(global-set-key [f5] 'org-capture)
(global-set-key [f6] 'org-agenda-list)
(global-set-key [f7] 'org-todo-list)

;;; add an annotation with the time when we change something's todo
;;; state
(setq org-log-done t)

(defun my-org-agenda-other-frame ()
  "Raise a frame with an org-agenda-list buffer in it.

Doesn't clean up after itself properly, but because you're using
emacs--daemon and invoking this function with emacsclient -e
\"(moaof)\", you can just roll your fingers over C-x C-c to close
the frame when you're done."
  "Create a new frame and run org-agenda."
  (interactive)
  (make-frame '((name . "Org-Agenda")
                (width  . 100)
                (height .  50)
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (auto-lower . nil)
                (auto-raise . t)))
  (select-frame-by-name "Org-Agenda")
  (org-agenda-list)
  (delete-other-windows))

(defun my-org-capture-other-frame ()
  "Raise a frame with an org-capture buffer in it. Close the
frame when org-capture is done."
  (interactive)
  (make-frame '((name . "Org-Capture")
                (width  . 100)
                (height .  50)
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (auto-lower . nil)
                (auto-raise . t)))
  (select-frame-by-name "Org-Capture")
  (if (condition-case nil
          (progn (org-capture) t)
        (error nil))
      (delete-other-windows) ;; if org-capture opens without error,
                             ;; give it some real estate
    (my-org-capture-other-frame-cleanup))) ;; if things blow up, close
                                           ;; the frame and clean up

;;; also close the window after org-capture succeeds
(add-hook 'org-capture-after-finalize-hook 'my-org-capture-other-frame-cleanup)

(defun my-org-capture-other-frame-cleanup ()
  "Close the Org-Capture frame."
  (if (equal "Org-Capture" (frame-parameter nil 'name))
      (delete-frame)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; appt-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; set up the appoinment notification facility and activate it
(setq
 appt-message-warning-time 45 ;warn 45 min in advance - time to get anywhere on campus
 appt-display-mode-line t
 appt-display-format 'window
 diary-file "~/org/diary")
(appt-activate 1)

;;; update appt every time we open our agenda
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;;; hook appt into the operating system's notification system. This
;;; will provide a series of notifications that start at
;;; appt-message-warning-time minutes before the appointment and go at
;;; two-ish-minute intervals until the appt actually happens.
(setq appt-disp-window-function (function saul-appt-display))

;;; on sane OSs, this will display a popup and play a chime.
(defun saul-appt-display (min-to-app new-time msg)
  (saul-notification-send (format "Appointment in %s minutes" min-to-app) msg
                          "/usr/share/icons/gnome/scalable/status/appoinment-soon-symbolic.png"
                          "/usr/share/sounds/ubuntu/stereo/message.ogg"))

(defun saul-notification-send (title msg &optional icon sound)
  "Show a popup if we're on X, otherwise echo it; TITLE is the title
  of the message, MSG is the content. Optionally provide a SOUND that
  will be played and an ICON that will be displayed."
  (interactive)
  (when sound (shell-command
               (concat "mplayer --really-quiet " sound " 2> /dev/null")))
  (if (eq window-system 'x)
      ;; visual version
      (shell-command (concat "notify-send "
                             (if icon (concat "-i " icon) "")
                             " '" title "' '" msg "'"))
    ;; text-only version
    (message (concat title ": " msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-latex)
(require 'org-special-blocks)
(setq org-export-latex-hyperref-format "\\ref{%s}")
(add-to-list 'org-export-latex-classes
             '("ieee"
               "\\documentclass[10pt,technote]{IEEEtran}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))             )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-babel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; load some languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (octave . t)
   (emacs-lisp . t)
   (lisp . t)))

(require 'org-context)
(org-context-activate)

;; Fix For Yasnippet

(defun yas/org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas-keymap [tab] 'yas/next-field)))

;; Recommended Key Bindings

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;Define a Dropbox location to save org notes
(setq org-directory "/home/ivan/Documents/org")

(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Start Flyspell with org-mode

(add-hook 'org-mode-hook 'turn-on-flyspell)
(org-ac/config-default)
(add-hook 'org-mode-hook 'ac-flyspell-workaround)

;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
