;; DECLARATIONS
(defun ze/this-file ()
  "Return path to this file."
  (cond
   (load-in-progress load-file-name)
   ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
    byte-compile-current-file)
   (:else (buffer-file-name))))
(defvar ze/required-packages
  '(dash)
  "Some packages are too good not to have.")

(setq user-emacs-directory (file-name-directory (file-truename (ze/this-file))))
;(add-to-list 'load-path user-emacs-directory)

;; Add external projects to load path. Note that anything installed
;; via package system will take precedence since dirs in elpa/ will
;; appear in `load-path' before site-lisp/ dirs and `package-install'
;; always loads files it installs. Either explicitly load customized
;; stuff before any packages or add their paths to `load-path' after
;; `package-initialize'. If ever in doubt which library took
;; precedence do `list-load-path-shadows'.
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))         

(defun packages-install (packages)
  (require 'dash)
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

;; PACKAGES
;; PACKAGES, PACKAGES
;; PACKAGES, PACKAGES, PACKAGES
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (file-exists-p (concat user-emacs-directory "elpa/archives/melpa"))
  (package-refresh-contents))

(let ((install #'(lambda (package)
                   (unless (package-installed-p package)
                     (package-install package))
                   (require package))))
  (message "Installing required packages %s" ze/required-packages)
  (mapc install ze/required-packages)
  (delete-other-windows))

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(use-package
      dash
      highlight-escape-sequences
      whitespace-cleanup-mode
      elisp-slime-nav
      smooth-scrolling
      shell-command
      easy-kill
      rainbow-mode
      diminish
      smartparens
      eval-sexp-fu)))

(init--install-packages)

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant
(require 'dash)
(require 'rx)

(eval-after-load "dash" '(dash-enable-font-lock))

(use-package f
  :ensure t)

(use-package s
  :ensure t)

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode
  :bind (("C-u" . undo-tree-undo)
         ("C-S-u" . undo-tree-redo)
         ("M-u" . undo-tree-visualize))
  :config (bind-keys :map undo-tree-map
                     ("C-/" . nil)
                     ("C-?" . nil)
                     ("C-_" . nil)))

;(require 'appearance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;          SETTINGS         ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Write backup files to own directory
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      ;; Make backups of files, even when they're in version control
      vc-make-backup-files t)

;; Save point position between sessions
(use-package saveplace
  :init
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  (setq-default save-place t))

(setq view-read-only t)

(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (exec-path-from-shell-initialize))

(define-minor-mode ze-mode
  "Ze mode to override other bindings."
  :init-value nil
  :lighter " ze"
  :keymap (make-sparse-keymap "ze"))

(bind-keys :prefix-map ze-prefix
           :prefix "<f3>"
           :prefix-docstring
           "Prefix for counsel / buffers / filesystem / windows-layout commands")

;(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-hippie)

(use-package yasnippet
  :ensure t
  :bind (("C-x t" . yas-expand))
  :init
  (progn
    (setq yas-verbosity 1
          yas-snippet-dirs `(,(expand-file-name "snippets" user-emacs-directory))
          yas-wrap-around-region t)

    (yas-global-mode 1))
  :config
  (progn
    ;; Inter-field navigation
    (defun yas/goto-end-of-active-field ()
      (interactive)
      (let* ((snippet (car (yas--snippets-at-point)))
             (position (yas--field-end (yas--snippet-active-field snippet))))
        (if (= (point) position)
            (move-end-of-line 1)
          (goto-char position))))

    (defun yas/goto-start-of-active-field ()
      (interactive)
      (let* ((snippet (car (yas--snippets-at-point)))
             (position (yas--field-start (yas--snippet-active-field snippet))))
        (if (= (point) position)
            (move-beginning-of-line 1)
          (goto-char position))))

    (bind-keys :map yas-keymap
               ("C-e" . yas/goto-end-of-active-field)
               ("C-a" . yas/goto-start-of-active-field)
               ("<return>" . yas/exit-all-snippets)
               :map yas-minor-mode-map
               ("<tab>" . nil)
               ("TAB" . nil)))
  :diminish yas-minor-mode)

(use-package datomic-snippets
  :ensure t)

(semantic-mode 1)

(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :bind (
         :map ze-prefix
         ("r" . ivy-resume)
         ("b" . ivy-switch-buffer)
         ;; TODO this one doesn't appear to respect persp
         ("B" . ivy-switch-buffer-other-window)
         :map ivy-minibuffer-map
         ("M-c" . ivy-kill-ring-save))
  :config
  (setq ivy-use-virtual-buffers t)
  :diminish ivy-mode)

(use-package ivy-hydra
  :ensure t
  :after ivy)

(use-package swiper
  :ensure t
  :bind ())

(use-package "isearch"
  ;; Defer because `isearch' is not a feature and we don't want to `require' it
  :defer t
  :init
  ;; `:diminish' doesn't work for isearch, because it uses eval-after-load on
  ;; the feature name, but isearch.el does not provide any feature.  For the
  ;; same reason we have to use `:init', but isearch is always loaded anyways.
  (diminish 'isearch-mode)
  (setq isearch-allow-scroll t)
  (bind-keys :map isearch-mode-map
             ("<escape>" . isearch-abort)
             ("C-q" . isearch-abort)))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :bind (
         :map ze-prefix
         ("d" . projectile-find-dir))
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-completion-system 'ivy
        projectile-find-dir-includes-top-level t
        projectile-indexing-method 'alien
        projectile-enable-caching nil)
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories ".node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".m2")
  (setq projectile-switch-project-action
        (lambda ()
          (dired (projectile-project-root))))

  :diminish projectile-mode)

(setq programming-modes
      '(clojure-mode emacs-lisp-mode racket-mode))

;; map files to modes
(require 'mode-mappings)

;; highlight escape sequences, works only in javascript
(use-package highlight-escape-sequences
  :ensure t
  :init (hes-mode))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Enable comment annotation keywords in programming modes
(comment-annotations-in-modes programming-modes)

(use-package expand-region
  :bind (("M-SPC" . er/expand-region))
  :ensure t
  :config
  (setq expand-region-contract-fast-key "-"
        expand-region-reset-fast-key "="))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ;;
;; STOPPED READING AFTER THIS ;;
;;                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (elisp-slime-nav-mode t)
            (eldoc-mode 1)
            (rainbow-mode +1)))

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

(require 'setup-smartparens)
(require 'smartparens-config)
(show-smartparens-global-mode)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(use-package recentf
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'"
                              "/elpa/.*\\'"
                              "/\\.node_modules/.*\\'"
                              ;; #'ignoramus-boring-p
                              )))

;; revert buffers automatically when underlying files are changed externally
(use-package autorevert
  :init (global-auto-revert-mode t)
  :config
  (setq auto-revert-verbose nil
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t)
  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil))
  :diminish auto-revert-mode)

(use-package re-builder
  :defer t
  :config (setq reb-re-syntax 'rx))

;; Elisp
(use-package elisp-mode
  :interpreter ("emacs" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eval-defun)
              ("C-c C-e" . eval-last-sexp)
              ("C-c C-k" . eval-buffer)))

(use-package macrostep
  :ensure t
  :after elisp-mode
  :bind (:map emacs-lisp-mode-map ("C-c m" . macrostep-expand)
         :map lisp-interaction-mode-map ("C-c m" . macrostep-expand)))

(use-package golden-ratio
  :ensure t
  :init
  (defun ze-toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :diminish golden-ratio-mode
  :config
  (progn
    (setq golden-ratio-exclude-modes '("bs-mode"
                                       "calc-mode"
                                       "ediff-mode"
                                       "gud-mode"
                                       "gdb-locals-mode"
                                       "gdb-registers-mode"
                                       "gdb-breakpoints-mode"
                                       "gdb-threads-mode"
                                       "gdb-frames-mode"
                                       "gdb-inferior-io-mode"
                                       "gud-mode"
                                       "gdb-inferior-io-mode"
                                       "gdb-disassembly-mode"
                                       "gdb-memory-mode"
                                       "restclient-mode"
                                       "speedbar-mode"
                                       ))

    (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")

    (setq golden-ratio-extra-commands
          (append golden-ratio-extra-commands
                  '(ace-window
                    ace-delete-window
                    ace-select-window
                    ace-swap-window
                    ace-maximize-window
                    avy-pop-mark
                    windmove-left
                    windmove-right
                    windmove-up
                    windmove-down
                    select-window-0
                    select-window-1
                    select-window-2
                    select-window-3
                    select-window-4
                    select-window-5
                    select-window-6
                    select-window-7
                    select-window-8
                    select-window-9
                    buf-move-left
                    buf-move-right
                    buf-move-up
                    buf-move-down
                    ess-eval-buffer-and-go
                    ess-eval-function-and-go
                    ess-eval-line-and-go
                    other-window
                    ze-other-window
                    quit-window)))))

;; clojure
(require 'setup-clj)

;; racket
(use-package racket-mode
  ;; :ensure t
  :load-path "site-lisp/racket-mode/"
  :mode (("\\.rkt\\'" . racket-mode))
  :config
  (add-hook 'racket-mode-hook)
  ;; :config (progn
  ;;           (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  ;;           (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable))

  ;; (lookup-key racket-mode-map (kbd "C-c C-e"))
  (bind-keys :map racket-mode-map
             ;; ("C-c m" . racket-macro-expand-map)
             ("C-c C-c" . racket-send-definition)
             ("C-c C-e" . racket-send-last-sexp)))


(use-package launch
  :ensure t
  :init
  (global-launch-mode +1))

(use-package avy-jump
  :ensure avy
  :init (bind-keys :prefix-map ze-nav-prefix
                   :prefix "M-n")
  :bind (("M-n c" . avy-goto-char-timer)
         ("M-n M-c" . avy-goto-char-timer)
         ("M-n w" . avy-goto-word-1)
         ("M-n SPC" . avy-goto-word-1)
         ;; ("M-n j" . avy-pop-mark)
         ("M-n j" . pop-to-mark-command)
         ("M-n a" . beginning-of-buffer)
         ("M-n e" . end-of-buffer))
  :config
  (setq avy-timeout-seconds 0.3))

(use-package jump-char
  :ensure t
  :init (bind-keys*
         ("C-." . jump-char-forward)
         ("C-," . jump-char-backward))
  :config
  (setq-default jump-char-forward-key "."
                jump-char-backward-key ","))


(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char))
  :config
  (setq zop-to-char-copy-keys '(?\M-c nil)
        zop-to-char-next-keys '(?\C-n nil)
        zop-to-char-prec-keys '(?\C-p nil)))

(bind-keys
 ; WHO DO THE FUCK THINKS THIS IS A GOOD IDEA
 ("C-z" . nil)
 ("C-<" . scroll-down-co)
 ("C->" . scroll-up-command)
 ;("<escape>" . bury-buffer)
 ("C-t" . hippie-expand-no-case-fold)
 ("C-<space>" . completion-at-point)
 ;("M-t" . completion-at-point)
 ("<f1>" . help-command)
 ("C-w" . kill-region-or-backward-word)
 ("<C-tab>" . ze-other-window)
 ("C-x <C-tab>" . i-meant-other-window)
 ("C-c C-e" . eval-and-replace)
 ("C-c c" . comment-or-uncomment-region-or-line)
 ("C-c d" . prelude-duplicate-current-line-or-region)
 ("C-c M-d" . prelude-duplicate-and-comment-current-line-or-region)
 ;("C-c j" . start-or-switch-to-shell)
 ;("C-c s" . create-scratch-buffer)
 ;("M-c" . easy-kill)
 ;("C-a" . prelude-move-beginning-of-line)
 ("C-x k" . kill-this-buffer)
 ("C-'" . quoted-insert))

(split-window-below)
(ze-toggle-golden-ratio)

