;; Latex Mode Initializer

(defun latex-mode-init ()
  "Calls Modes used when initializing a Latex buffer."
  (visual-line-mode)
  (flyspell-mode)
  (LaTeX-math-mode)
  (turn-on-reftex)
  (ac-LaTeX-mode-setup)
  (ac-flyspell-workaround)
  (magic-latex-buffer)
  (latex-preview-pane-mode)
  )

(add-hook 'LaTeX-mode-hook 'latex-mode-init)
