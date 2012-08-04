;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;;  ORG-MODE CONFIGURATIONS                                                ;;
;;                                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-install)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load org-mode for .org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; useful hotkeys
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-a") 'org-agenda)
(global-set-key [f8] 'org-todo-list)

;; add an annotation with the time when we change something's todo
;; state
(setq org-log-done t)

;; add some extra todo keywords
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "WILL-NOT-DO" "REVISIT-LATER")))

(defun my-org-agenda-other-frame ()
  "Raise a frame with an org-agenda-list buffer in it.

Doesn't clean up after itself, but because you're using
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

;; also close the window after org-capture succeeds
(add-hook 'org-capture-after-finalize-hook 'my-org-capture-other-frame-cleanup)

(defun my-org-capture-other-frame-cleanup ()
  "Close the Org-Capture frame."
  (if (equal "Org-Capture" (frame-parameter nil 'name))
      (delete-frame)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; appt-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set up the appoinment notification facility and activate it
(setq
 appt-message-warning-time 30           ;warn 45 min in advance - time to get anywhere on campus
 appt-display-mode-line t
 appt-display-format 'window
 diary-file "~/org/diary")
(appt-activate 1)

;; update appt every time we open our agenda
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; hook appt into the operating system's notification system. On sane
;; OSs, this will display a popup and play a chime.
(defun saul-appt-display (min-to-app new-time msg)
  (saul-notification-send (format "Appointment in %s minutes" min-to-app) msg
                          "/usr/share/icons/gnome/scalable/status/appoinment-soon-symbolic.png"
                          "/usr/share/sounds/ubuntu/stereo/message.ogg"))
(setq appt-disp-window-function (function saul-appt-display))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mobile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; update everything on startup
(org-mobile-pull)
(org-mobile-push)

(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

;; this function waits until Emacs has been idle for n seconds and
;; then pushes. This means that if you queue up a giant batch of
;; changes all at once (for example, hitting C-x C-s on your agenda
;; list to save all org files), only one push is made.
(defun org-mobile-push-with-delay (secs)
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))

(defun install-file-monitor (file secs)
  (run-with-timer
   0 secs
   (lambda (f p)
     (unless (< p (second (time-since (elt (file-attributes f) 5))))
       (org-mobile-pull)))
   file secs))

;; whenever we save an org-mode file, wait thirty seconds, and if no
;; more changes have been made, push them to the 
(add-hook 'after-save-hook 
          (lambda () 
            (when (eq major-mode 'org-mode)
              (dolist (file (org-mobile-files-alist))
                (if (string= (expand-file-name (car file)) (buffer-file-name))
                    (org-mobile-push-with-delay 30))))))

;; Blindly push once a day in case our org-mode files are edited from
;; outside emacs
(run-at-time
 "00:05" (* 60 60 24)
 '(lambda () (org-mobile-push-with-delay 1)))

;; install a file monitor that checks for new changes to pull every
;; fifteen seconds
(install-file-monitor (file-truename
                       (concat
                        (file-name-as-directory org-mobile-directory)
                        org-mobile-capture-file))
                      15)

;; Blindly pull every 5 minutes in case there are problems with
;; timestamping (i.e. dropbox bugs)
(run-with-timer
 0 (* 5 60)
 'org-mobile-pull)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-babel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load some languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (octave . t)
   (emacs-lisp . t)
   (lisp . t)))

