;;; magit-log-edit.el --- Major mode for editing git commit messages

;; This software is Copyright (c) 2010 by Florian Ragwitz.
;;
;; This is free software, licensed under:
;;   The GNU General Public License, Version 2, June 1991

;; Author: Florian Ragwitz <rafl@debian.org>
;; Version: 0.1
;; Keywords: convenience git

;;; History:
;;
;;  0.1   Tue, 06 Jul 2010 18:54:35 +0200
;;    * Initial version
;;

;;; Commentary:
;;

;;; Code:

(defgroup magit-log-edit '((jit-lock custom-group))
  "Mode for editing git commit messages from magit"
  :group 'faces)

(defgroup magit-log-edit-faces nil
  "Faces for highlighting git commit messages from magit"
  :prefix "magit-log-edit-"
  :group 'magit-log-edit)

(defface magit-log-edit-summary-face
  '((default (:weight bold))
    (((class grayscale) (background light))
     (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light))
     (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight the summary in git commit messages"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-overlong-summary-face
  '((((class color) (min-colors 88) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 88) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 16) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 16) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t :weight bold)))
  "Face used to highlight overlong parts of git commit message summaries"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-nonempty-second-line-face
  '((((class color) (min-colors 88) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 88) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 16) (background light))
     (:foreground "Red1" :weight bold))
    (((class color) (min-colors 16) (background dark))
     (:foreground "Pink" :weight bold))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t :weight bold)))
  "Face used to highlight text on the second line of git commit messages"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-text-face
  '((t (:inherit default)))
  "Face used to highlight text in git commit messages"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-comment-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :weight bold :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :weight bold :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "Firebrick"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "chocolate1"))
    (((class color) (min-colors 16) (background light))
     (:foreground "red"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "red1"))
    (((class color) (min-colors 8) (background light))
     (:foreground "red"))
    (((class color) (min-colors 8) (background dark)))
    (t (:weight bold :slant italic)))
  "Face used to highlight comments in git commit messages"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-pseudo-header-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Font used to hightlight pseudo headers in git commit messages"
  :group 'magit-log-edit-faces)

(defcustom magit-log-edit-known-pseudo-headers
  '("Signed-off-by"
    "Acked-by"
    "Cc"
    "Reported-by"
    "Tested-by"
    "Reviewed-by")
  "A list of git pseudo headers to be highlighted."
  :group 'magit-log-edit
  :type '(repeat string))

(defface magit-log-edit-known-pseudo-header-face
  '((((class grayscale) (background light)) (:foreground "Gray90" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light))
     (:foreground "ForestGreen"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "PaleGreen"))
    (((class color) (min-colors 16) (background light))
     (:foreground "ForestGreen"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "PaleGreen"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:weight bold :underline t)))
  "Face used to hightlight common pseudo headers in git commit messages"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-note-brace-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :weight bold :underline t))
    (((class color) (min-colors 88) (background light))
     (:foreground "dark cyan"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "Aquamarine"))
    (((class color) (min-colors 16) (background light))
     (:foreground "CadetBlue"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "Aquamarine"))
    (((class color) (min-colors 8)) (:foreground "magenta"))
    (t (:weight bold :underline t)))
  "Face used to highlight braces within notes in git commit messages"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-note-address-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :weight bold))
    (((class grayscale) (background dark)) (:foreground "DimGray" :weight bold))
    (((class color) (min-colors 88) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1"))
    (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
    (((class color) (min-colors 16) (background dark)) (:foreground "Cyan"))
    (((class color) (min-colors 8)) (:foreground "cyan" :weight bold))
    (t (:weight bold)))
  "Face used to highlight email addresses within notes in git commit messages"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-note-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "VioletRed4"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light))
     (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight notes within git commit messages"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-branch-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :slant italic))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :slant italic))
    (((class color) (min-colors 88) (background light))
     (:foreground "VioletRed2"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 16) (background light))
     (:foreground "RosyBrown"))
    (((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t (:slant italic)))
  "Face used to highlight the branch name in comments in git commit messages"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-no-branch-face
  '((t :inherit magit-log-edit-branch-face))
  "Face used when a commit is going to be made outside of any branches"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-comment-heading-face
  '((t (:inherit magit-log-edit-known-pseudo-header-face)))
  "Face used to highlight section headings in the default
comments in git commit messages"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-comment-file-face
  '((t (:inherit magit-log-edit-pseudo-header-face)))
  "Face used to highlight file names in the default comments in
git commit messages"
  :group 'magit-log-edit-faces)

(defface magit-log-edit-comment-action-face
  '((t (:inherit magit-log-edit-branch-face)))
  "Face used to highlight what has happened to files in the
default comments in git commit messages"
  :group 'magit-log-edit-faces)

(defconst magit-log-edit-font-lock-keywords-1
  (append
   '(("^\\(#\s+On branch \\)\\(.*\\)$"
      (1 'magit-log-edit-comment-face)
      (2 'magit-log-edit-branch-face)))
   (loop for exp in
         '(("Not currently on any branch." . magit-log-edit-no-branch-face)
           ("Changes to be committed:"     . magit-log-edit-comment-heading-face)
           ("Untracked files:"             . magit-log-edit-comment-heading-face)
           ("Changed but not updated:"     . magit-log-edit-comment-heading-face)
           ("Unmerged paths:"              . magit-log-edit-comment-heading-face))
         collect `(,(concat "^\\(#\s+\\)\\(" (car exp) "\\)$")
                   (1 'magit-log-edit-comment-face)
                   (2 ',(cdr exp))))
   `(("^\\(#\t\\)\\([^:]+\\)\\(:\s+\\)\\(.*\\)$"
      (1 'magit-log-edit-comment-face)
      (2 'magit-log-edit-comment-action-face)
      (3 'magit-log-edit-comment-face)
      (4 'magit-log-edit-comment-file-face))
     ("^\\(#\t\\)\\(.*\\)$"
      (1 'magit-log-edit-comment-face)
      (2 'magit-log-edit-comment-file-face))
     ("^#.*$"
      (0 'magit-log-edit-comment-face))
     ("\\`\\(?:\\(?:[[:space:]]*\\|#.*\\)\n\\)*\\(.\\{,50\\}\\)\\(.*?\\)\\(?:\n\\(.*\\)\\)?$"
      (1 'magit-log-edit-summary-face)
      (2 'magit-log-edit-overlong-summary-face)
      (3 'magit-log-edit-nonempty-second-line-face))
     (,(concat "^\\("
               (regexp-opt magit-log-edit-known-pseudo-headers)
               ":\\)\\(\s.*\\)$")
      (1 'magit-log-edit-known-pseudo-header-face)
      (2 'magit-log-edit-pseudo-header-face))
     ("^\\w[^\s\n]+:\s.*$"
      (0 'magit-log-edit-pseudo-header-face))
     ("^\\(\\[\\)\\([^\s@]+@[^\s@]+:\\)\\(.*\\)\\(\\]\\)$"
      (1 'magit-log-edit-note-brace-face)
      (2 'magit-log-edit-note-address-face)
      (3 'magit-log-edit-note-face)
      (4 'magit-log-edit-note-brace-face))
     (".*"
      (0 'magit-log-edit-text-face)))))

(defvar magit-log-edit-font-lock-keywords magit-log-edit-font-lock-keywords-1)

(defvar magit-log-edit-mode-hook nil
  "List of functions to be called when activating `magit-log-edit-mode'.")

(defun magit-log-edit-git-config-var (key)
  "Retrieve a git configuration value.
Invokes 'git config --get' to retrieve the value for the
configuration key KEY."
  (let* ((exit)
        (output
         (with-output-to-string
           (with-current-buffer
               standard-output
             (setq exit
                   (call-process "git" nil (list t nil) nil
                                 "config" "--get" key))))))
    (if (not (= 0 exit))
        nil
      (substring output 0 (- (length output) 1)))))

(defun magit-log-edit-first-env-var (&rest vars)
  "Get the value of the first defined environment variable.
Walk VARS, call `getenv' on each element and return the first
non-nil return value of `getenv'."
  (loop for var in vars
        do (let ((val (getenv var)))
             (when val (return val)))))

(defun magit-log-edit-committer-name ()
  "Get the git committer name of the current user.
This uses the same mechanism git itself uses.  That is, using the
value of the 'GIT_AUTHOR_NAME' or 'GIT_COMMITTER_NAME'
environment variables, or the 'user.name' git configuration
variable.

If the above mechanism fails, the value of the variable
`user-full-name' is used."
  (or
   (magit-log-edit-first-env-var "GIT_AUTHOR_NAME" "GIT_COMMITTER_NAME")
   (magit-log-edit-git-config-var "user.name")
   user-full-name))

(defun magit-log-edit-committer-email ()
  "Get the git committer email address of the current user.
This uses the same mechanism git itself uses.  That is, using the
value of the 'GIT_AUTHOR_EMAIL', 'GIT_COMMITTER_EMAIL', or
'EMAIL' environment variables, or the 'user.email' git
configuration variable.

If the above mechanism fails, the value of the variable
`user-email-address' is used."
  (or
   (magit-log-edit-first-env-var "GIT_AUTHOR_EMAIL" "GIT_COMMITTER_EMAIL" "EMAIL")
   (magit-log-edit-git-config-var "user.email")
   user-mail-address))

(defun magit-log-edit-find-pseudo-header-position ()
  "Find the position at which commit pseudo headers should be inserted.
Those headers usually live at the end of a commit message, but
before any trailing comments git or the user might have inserted."
  (save-excursion
    ;; skip the summary line, limit the search to comment region
    (goto-char (point-min))
    (forward-line 2)
    (let ((comment-start (point)))
      (goto-char (point-max))
      (if (not (re-search-backward "^[^#][^\s:]+:.*$" comment-start t))
          ;; no headers yet, so we'll search backwards for a good place
          ;; to insert them
          (if (not (re-search-backward "^[^#].*?.*$" comment-start t))
              ;; no comment lines anywhere before end-of-buffer, so we
              ;; want to insert right there
              (point-max)
            ;; there's some comments at the end, so we want to insert
            ;; before those
            (beginning-of-line)
            (forward-line 1)
            (point))
        ;; we're at the last header, and we want the line right after
        ;; that to insert further headers
        (beginning-of-line)
        (forward-line 1)
        (point)))))

(defun magit-log-edit-insert-header (type name email &optional note)
  "Insert a header into the commit message.
The inserted headers have the format 'TYPE: NAME <EMAIL>'.

If NOTE satisfies `stringp', an additional note of the format
'[EMAIL: NOTE]' is inserted after the header.

If NOTE is not nil and doesn't satisfy `stringp', the
surroundings of an additional note will be inserted, and the
point will be left where the content of the note needs to be
inserted.

The header is inserted at the position returned by
`magit-log-edit-find-pseudo-header-position'.  When this position
isn't after an existing header or a newline, an extra newline is
inserted before the header."
  (let* ((header-at (magit-log-edit-find-pseudo-header-position))
         (prev-line (save-excursion
                      (goto-char (- header-at 1))
                      (thing-at-point 'line)))
         (pre       (if (or (string-match "^[^\s:]+:.+$" prev-line)
                            (string-match "\\`\s*$" prev-line))
                        "" "\n"))
         (insert    (lambda ()
                      (goto-char header-at)
                      (insert (format "%s%s: %s <%s>\n" pre type name email))
                      (when note
                        (insert (format "[%s: %s]\n"
                                        email (if (stringp note) note "")))
                        (backward-char 2)))))
    (if (eq t note)
        (funcall insert)
      (save-excursion (funcall insert)))))

(defun magit-log-edit-insert-header-as-self (type &optional note)
  "Insert a header with the name and email address of the current user.
Call `magit-log-edit-insert-header' with the user name and email
address provided by `magit-log-edit-committer-name' and
`magit-log-edit-committer-email'.

TYPE and NOTE are passed along unmodified."
  (let ((committer-name (magit-log-edit-committer-name))
        (committer-email (magit-log-edit-committer-email)))
    (magit-log-edit-insert-header type committer-name committer-email note)))

(defun magit-log-edit-font-lock-diff ()
  "Add font lock on diff."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^diff --git" nil t)
      (let ((beg (match-beginning 0)))
        (let* ((buffer (current-buffer))
               (font-lock-verbose nil)
               (font-lock-support-mode nil)
               (text (with-temp-buffer
                       (insert
                        (with-current-buffer buffer
                          (buffer-substring-no-properties beg (point-max))))
                       (diff-mode)
                       (font-lock-fontify-buffer)
                       (let ((pos (point-min))
                             next)
                         (while (setq next (next-single-property-change pos 'face))
                           (put-text-property pos next 'font-lock-face
                                              (get-text-property pos 'face))
                           (setq pos next)))
                       (buffer-string))))
          (delete-region beg (point-max))
          (insert text))))))

;;; magit-log-edit.el ends here
