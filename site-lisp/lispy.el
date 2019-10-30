
;; lispy.el
(use-package lispy
  :ensure t
  :defer t
  :diminish
  (lispy-mode . "(λ)")
  :init
  (progn
    (defun ze-emacs-lisp-hook ()
      (lispy-mode 1))

    (add-hook 'emacs-lisp-mode-hook 'ze-emacs-lisp-hook))
  :config
  (progn
    ;; lispy-safe are must have settings.
    (setq lispy-safe-copy t
          lispy-safe-delete t
          lispy-safe-paste t)
    (defun sexy-form-state-p ()
      (or (region-active-p)
          (and (not (lispy--in-string-or-comment-p))
               (or (looking-back "['`~@#%]")
                   (and (looking-at "\\(?:[[:space:]]+\\)")
                        (or (looking-back lispy-left)
                            (looking-back "\\(?:^\\|[[:space:]]+\\)")))
                   (and (looking-at lispy-right)
                        (or (looking-back "\\(?:^\\|[[:space:]]+\\)")
                            (looking-back lispy-left)))
                   (and (looking-at "\\(?:$\\|[[:space:]]+\\)")
                        (looking-back "\\(?:^\\|[[:space:]]+\\)"))))))
    (defun sexy-parens ()
      (interactive)
      (cond
       ((sexy-form-state-p) (call-interactively 'lispy-parens))
       (t (call-interactively 'lispy-backward))))

    (defun sexy--skip-string-or-comment-down (arg)
      (interactive "p")
      (cond ((lispy--in-string-p)
             (goto-char (cdr (lispy--bounds-string))))
            ((lispy--in-comment-p)
             (progn
               (goto-char (1+ (cdr (lispy--bounds-comment))))
               (skip-chars-forward "\n")
               (forward-sexp 1)
               (forward-sexp -1)))))

    (defun sexy--skip-string-or-comment-up (arg)
      (interactive "p")
      (cond ((lispy--in-string-p)
             (goto-char (car (lispy--bounds-string))))
            ((lispy--in-comment-p)
             (progn
               (goto-char (1- ((lispy--bounds-comment))))
               (skip-chars-backward "\n")
               (forward-sexp -1)
               (forward-sexp 1)))))

    (defun sexy-try ()
      ""
      (interactive)
      (when (lispy-looking-back "\\sw #")
        (progn
          (backward-delete-char 2)
          (insert "#"))))

    (defun sexy-forward-line-column ()
      (interactive)
      (let ((c (current-column)))
        (forward-line 1)
        (move-to-column c)
        (= c (current-column))))

    (defun sexy-find-lispy-left-down (arg)
      (interactive "p")
      (let ((p (point))
            (c (current-column))
            (lispy-thing (cond
                          ((lispy-left-p) #'lispy-left-p)
                          ((lispy-right-p) #'lispy-right-p)
                          (t nil)))
            (end (buffer-end 1))
            (sexy-found-p nil))
        (when lispy-thing
          (while (and (not sexy-found-p)
                      (not (= (point) end))
                      (forward-line 1))
            (when (and (= c (move-to-column c))
                       (funcall lispy-thing))
              (setq sexy-found-p t)))
          (when (not (and (funcall lispy-thing)
                          (= c (current-column))))
            (goto-char p)))))

    (defun sexy-into-sexp (arg)
      (interactive "p")
      (cond ((lispy-right-p)
             (backward-char 1)
             ;; (insert " ")
             )
            ((lispy-left-p)
             (forward-char 1)
             ;; (insert " ")
             ;; (backward-char 1)
             )))

    (defun sexy-tab (arg)
      ""
      (interactive "p")
      (lispy-tab)
      (indent-for-tab-command arg))

    ;; TODO need to figure out this tabbing stuff
    ;; Experimental lispy bindings
    ;; (lispy-define-key lispy-mode-map-special "i" 'sexy-into-sexp)
    ;; (bind-keys :map lispy-mode-map
    ;;            ("<tab>" . sexy-tab)
    ;;            ;; ("<C-i>" . sexy-tab)
    ;;            )
    ;; (lispy-define-key lispy-mode-map "<C-i>" 'sexy-tab)
    ;; (lispy-define-key lispy-mode-map "<tab>" 'sexy-tab)

    (defun sexy-next-thing (arg)
      ""
      (interactive "p")
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (sexy--skip-string-or-comment-down 1)
      (forward-sexp 1))

    (defun sexy-previous-thing (arg)
      ""
      (interactive "p")
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (sexy--skip-string-or-comment-up 1)
      (backward-sexp 1))

    (defun sexy-next-thing (arg)
      ""
      (interactive "p")
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (sexy-out-of-string-or-comment 1)
      (forward-sexp 1))

    (defun sexy-next-thing (arg)
      ""
      (interactive "p")
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (when (lispy--in-string-or-comment-p)
        (forward-sexp))
      (unless (looking-at-p lispy-right)
        (sp-select-next-thing)
        (lispy-down arg)
        (when (region-active-p)
          (deactivate-mark))))

    (defun sexy-previous-thing (arg)
      ""
      (interactive "p")
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (unless (looking-back lispy-left)
        (sp-select-previous-thing)
        (lispy-up arg)
        (when (region-active-p)
          (deactivate-mark))))

    (defun sexy-prev-paragraph ()
      ""
      (interactive)
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (if (= (point)
             (progn (lispy-beginning-of-defun)
                    (point)))
          (lispy-up 1)))

    (defun sexy-next-paragraph ()
      ""
      (interactive)
      (when (called-interactively-p 'interactive)
        (lispy--remember))
      (lispy-beginning-of-defun)
      (lispy-down 1))

    (defun sexy-kill-region-or-backward-word (arg)
      (interactive "p")
      (if (region-active-p)
          (lispy-kill-at-point)
        (lispy-backward-kill-word arg)))

    (turn-on-smartparens-strict-mode)
    (setq lispy-visit-method 'projectile)

    (defun sexy-move-beginning-of-line (arg)
      "Back to indentation, else fwd to `move-beginning-of-line'.
Reveal outlines."
      (interactive "^p")
      (lispy--ensure-visible)
      (prelude-move-beginning-of-line arg))

    (bind-keys :map lispy-mode-map
               ;; was lispy-left
               ("M-n" . nil)
               ("C-a" . sexy-move-beginning-of-line)
               ("{" . nil)
               ("}" . nil)
               ;; Don't mess with Command keys
               ("C-S-f" . nil)
               ("C-S-b" . nil)
               ("(" . sexy-parens)
               (")" . lispy-forward)
               ("[" . lispy-brackets)
               ("]" . lispy-right)
               ("n" . special-lispy-down)
               ("p" . special-lispy-up)
               ;; was lispy-clone
               ("c" . special-lispy-new-copy)
               ;; was lispy-convolute
               ("C" . special-lispy-clone)
               ("C-u" . undo-tree-undo)
               ([remap sp-backward-kill-word] . sexy-kill-region-or-backward-word)
               :map lispy-mode-map-lispy
               ;; was lispy-kill-at-point
               ("C-," . nil)
               ("C-w" . lispy-kill-at-point)
               ("M-h" . sexy-kill-region-or-backward-word))))

