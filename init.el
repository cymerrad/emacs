;;;; Paths

(prefer-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(setq iso-transl-char-map nil)          ; http://emacs.stackexchange.com/a/17524/2138

(defvar user-setup-directory          (expand-file-name "setup"          user-emacs-directory))
(defvar user-setup-builtins-directory (expand-file-name "setup/builtins" user-emacs-directory))
(defvar local-dev-package-directory   (expand-file-name "packages"       user-emacs-directory))
(defvar user-data-directory           (expand-file-name ""               user-emacs-directory))
(defvar user-cache-directory          (expand-file-name ".cache"         user-emacs-directory))
(defvar user-bin-directory            (expand-file-name "bin"            "~"))
(setq custom-file                     (expand-file-name "settings.el"    user-emacs-directory))
(make-directory user-cache-directory t)


;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;;    [default default default italic underline success warning error])
;;  '(custom-enabled-themes (quote (solarized-dark)))
;;  '(custom-safe-themes
;;    (quote
;;     ("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default)))
;;  '(inhibit-startup-screen t)
;;  '(package-selected-packages
;;    (quote
;;     (solarized-theme zop-to-char page-break-lines jump-char launch lua-mode clj-refactor cider align-cljlet golden-ratio lispy macrostep expand-region persp-mode projectile swiper ivy-hydra ivy datomic-snippets exec-path-from-shell racket-mode))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))



;;; Requires
(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))

(use-package benchmark-init
  :ensure t)

(require 'diminish)
(require 'bind-key)

(require 'subr-x)
(require 'time-date)

;; And disable the site default settings
(setq inhibit-default-init t)



;;; Customization
(defconst custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (setq custom-file custom-file
        custom-buffer-done-kill nil            ; Kill when existing
        custom-buffer-verbose-help nil         ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load custom-file 'no-error 'no-message))



;;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; (when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  ;; (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))


;; No blinking and beeping, no startup screen, no scratch message and short
;; Yes/No questions.
(blink-cursor-mode -1)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      initial-scratch-message "Hello there!\n")
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

(use-package solarized                  ; My colour theme
  :ensure solarized-theme
  :config
  ;; Disable variable pitch fonts in Solarized theme
  (setq solarized-use-variable-pitch nil
        ;; Prefer italics over bold
        solarized-use-less-bold t
        solarized-use-more-italic t
        solarized-distinct-doc-face t ; Emphasize docstrings
        ;; I find different font sizes irritating.
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)

  (load-theme 'solarized-dark 'no-confirm))

(bind-key "C-c t v" #'variable-pitch-mode)

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package beacon                     ; Highlight cursor position in buffer
  :ensure t
  :init (beacon-mode 1)
  :diminish beacon-mode)

(use-package stripe-buffer              ; Add stripes to a buffer
  :disabled t
  :ensure t
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode))




;;; Keys and key bindings

;; Our key bindings are solely in the user space C-c <letter>.  The list of
;; which-key prefixes documents the meaning of specific key prefixes, such as
;; C-c f for file commands.  C-c m is special in that it always holds commands
;; that are only for the current major mode.  The mode-specific which-key
;; prefixes document the individual bindings for major modes under C-c m.
;;
;; We may also bind F5 to F9.  Since we can't document these in code, the
;; following list shows their abstract meanings:
;;
;; * F5: Compile
;; * F6: n/a
;; * F7: n/a
;; * F8: n/a
;; * F9: n/a
;;
;; All of these keys have default global bindings, but major and minor modes may
;; override them if there's a better command for the specific purpose available.
;;
;; Note that the notation for the function keys is <f5>, i.e. the lowercase name
;; surrounded by angle brackets.

(use-package which-key                  ; Show help popups for prefix keys
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        ;; Let's go unicode :)
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←")
          ("DEL"                   . "⌫")
          ("deletechar"            . "⌦")
          ("RET"                   . "⏎"))
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ;; Lambdas
          ("\\`\\?\\?\\'"   . "λ")
          ;; Prettify hydra entry points
          ("/body\\'"       . "|=")
          ;; Drop/shorten package prefixes
          ("projectile-"    . "proj-")
          ("helm-"          . "h-")
          ("magit-"         . "ma-")))

  (which-key-declare-prefixes
    ;; Prefixes for global prefixes and minor modes
    "C-c @" "outline"
    "C-c !" "flycheck"
    "C-c 8" "typo"
    "C-c 8 -" "typo/dashes"
    "C-c 8 <" "typo/left-brackets"
    "C-c 8 >" "typo/right-brackets"
    ;; Prefixes for my personal bindings
    "C-c a" "applications"
    "C-c b" "buffers"
    "C-c c" "compile-and-comments"
    "C-c e" "errors"
    "C-c f" "files"
    "C-c f v" "variables"
    "C-c g" "git"
    "C-c g g" "github/gist"
    "C-c h" "helm/help"
    "C-c i" "insert"
    "C-c i l" "licenses"
    "C-c j" "jump"
    "C-c l" "language/spelling"
    "C-c m" "major mode"
    "C-c o" "cursors"
    "C-c p" "projects"
    "C-c p s" "projects/search"
    "C-c p x" "projects/execute"
    "C-c p 4" "projects/other-window"
    "C-c s" "search"
    "C-c t" "toggle"
    "C-c w" "windows/frames"
    "C-c x" "text")

  ;; Prefixes for major modes
  (which-key-declare-prefixes-for-mode 'markdown-mode
    "C-c TAB" "markdown/images"
    "C-c C-a" "markdown/links"
    "C-c C-c" "markdown/process"
    "C-c C-s" "markdown/style"
    "C-c C-t" "markdown/header"
    "C-c C-x" "markdown/structure"
    "C-c m" "markdown/personal")

  (which-key-declare-prefixes-for-mode 'emacs-lisp-mode
    "C-c m" "elisp/personal"
    "C-c m e" "eval")

  (which-key-declare-prefixes-for-mode 'js2-mode
    "C-c m" "js/personal"
    "C-c m r" "refactor")

  (which-key-declare-prefixes-for-mode 'scala-mode
    "C-c C-b" "ensime/build"
    "C-c C-d" "ensime/debug"
    "C-c C-r" "ensime/refactor"
    "C-c C-v" "ensime/misc"
    "C-c m" "scala/personal"
    "C-c m b" "scala/build")

  (which-key-declare-prefixes-for-mode 'haskell-mode
    "C-c m" "haskell/personal"
    "C-c m i" "haskell/imports")

  (which-key-declare-prefixes-for-mode 'rust-mode
    "C-c C-c" "rust/cargo")

  (which-key-declare-prefixes-for-mode 'web-mode
    "C-c C-a" "web/attributes"
    "C-c C-b" "web/blocks"
    "C-c C-d" "web/dom"
    "C-c C-e" "web/element"
    "C-c C-t" "web/tags")

  :diminish which-key-mode)

(use-package hydra                      ; Bindings that stick
  :ensure t)

(use-package helm-descbinds             ; Describe key bindings with Helm
  :ensure t
  :bind ("C-h C-b" . describe-bindings)
  :init (helm-descbinds-mode))

(use-package help
  :config
  (defun describe-key-copy-as-kill (&optional key insert untranslated)
    (kill-new (format "%s" (key-binding key))))
  (advice-add 'describe-key-briefly :after #'describe-key-copy-as-kill))



;; Package manager and init file
(use-package paradox                    ; Better package menu
  :ensure t
  :bind (("C-c a p" . paradox-list-packages)
         ("C-c a P" . package-list-packages-no-fetch))
  :config
  (setq paradox-execute-asynchronously nil ; No async update, please
        paradox-spinner-type 'moon      ; Fancy spinner
        ;; Show all possible counts
        paradox-display-download-count t
        paradox-display-star-count t
        ;; Don't star automatically
        paradox-automatically-star nil
        ;; Hide download button, and wiki packages
        paradox-use-homepage-buttons nil ; Can type v instead
        paradox-hide-wiki-packages t))

(use-package bug-hunter                 ; Search init file for bugs
  :ensure t)



  ;;; The mode line
(line-number-mode)
(column-number-mode)

(use-package fancy-battery              ; Fancy battery info for mode line
  :ensure t
  :defer t
  :init (fancy-battery-mode))

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (global-anzu-mode)
  :config (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

(use-package which-func                 ; Current function name
  :init (which-function-mode)
  :config
  (setq which-func-unknown "⊥" ; The default is really boring…
        which-func-format
        `((:propertize (" ➤ " which-func-current)
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))


(use-package powerline                  ; The work-horse of Spaceline
  :ensure t
  :after spaceline-config
  :config (setq powerline-height (truncate (* 1.0 (frame-char-height)))
                powerline-default-separator 'utf-8))



;;; Minibuffer and Helm
(setq history-length 1000               ; Store more history
      use-dialog-box nil                ; Never use dialogs for minibuffer input
      )

(use-package savehist                   ; Save minibuffer history
  :init (savehist-mode t)
  :config (setq savehist-save-minibuffer-history t
                savehist-autosave-interval 180))

(bind-keys
 ("C-z" . nil) ;; WHO THE FUCK THINKS STOPPING APPLICATION WITH THIS SHORTCUT IS A GOOD IDEA
; ("C-<" . scroll-down-co)
; ("C->" . scroll-up-command)
 ;("<escape>" . bury-buffer)
; ("C-t" . hippie-expand-no-case-fold)
; ("C-SPC" . completion-at-point)
 ;("M-t" . completion-at-point)
; ("<f1>" . help-command)
; ("C-w" . kill-region-or-backward-word)
 ("C-w" . backward-kill-word)
; ("<C-tab>" . ze-other-window)
; ("C-x <C-tab>" . i-meant-other-window)
; ("C-c C-e" . eval-and-replace)
 ("C-/" . comment-or-uncomment-region-or-line)
; ("C-c d" . prelude-duplicate-current-line-or-region)
; ("C-c M-d" . prelude-duplicate-and-comment-current-line-or-region)
 ;("C-c j" . start-or-switch-to-shell)
 ;("C-c s" . create-scratch-buffer)
 ;("M-c" . easy-kill)
 ;("C-a" . prelude-move-beginning-of-line)
 ("C-x k" . kill-this-buffer))

(use-package helm                       ; Powerful minibuffer input framework
  :ensure t
  :bind (("A-C-o"   . helm-resume)
         ("C-x C-f" . helm-find-files)
         ("A-f"     . helm-find)
         ("C-h SPC" . helm-all-mark-rings))
  :init
  ;; FIXME: these are experimental convenience keybindings used by aculich that
  ;; may not be more generally useful nor wanted. Revisit these in the future.
  (bind-keys
   :map helm-map
   ("C-w" . backward-kill-word)
   ("M-w" . helm-yank-text-at-point)
   ("C-z" . helm-select-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-j" . helm-maybe-exit-minibuffer)
   ("C-<return>" . helm-execute-persistent-action))

  (bind-keys
     :prefix-map help-helm-map
     :prefix "C-c h"
     ("a" . helm-apropos)
     ("b" . helm-buffers-list)
     ("c" . helm-colors)
     ("e" . helm-register)
     ("f" . helm-find-files)
     ("g" . helm-git-grep)
     ("i" . helm-semantic-or-imenu)
     ("k" . helm-man-woman)
     ("m" . helm-all-mark-rings)
     ("o" . helm-occur)
     ("O" . helm-multi-occur)
     ("p" . helm-list-emacs-process)
     ("r" . helm-regexp)
     ("l" . helm-resume)
     ("R" . helm-resume)
     ("t" . helm-top)
     ("y" . helm-show-kill-ring)
     ("/" . helm-find))
  (helm-mode 1)
  (with-eval-after-load 'helm-config
    (warn "`helm-config' loaded! Get rid of it ASAP!"))
  :config
  ;; Split inside selected window with Helm
  (setq helm-split-window-in-side-p t)

  ;; FIXME: still experimenting with the right combination of settings
  ;; here. Suggestions welcome.
  (setq
   helm-always-two-windows nil

   helm-truncate-lines t
   helm-full-frame nil
   helm-split-window-default-side 'left
   helm-reuse-last-window-split-state t
   helm-split-window-in-side-p nil
   helm-ff-file-name-history-use-recentf t
   helm-ff-search-library-in-sexp t
   ;; helm-buffers-fuzzy-matching t
   ;; helm-man-or-woman-function 'woman
   ;; helm-quick-update t
   )

  :diminish helm-mode)

(use-package helm-command               ; Command execution with Helm
  :ensure helm
  :defer t
  :bind (([remap execute-extended-command] . helm-M-x)))

(use-package focus-autosave-mode        ; Save buffers when focus is lost
  :ensure t
  :init (focus-autosave-mode)
  :diminish focus-autosave-mode)

(use-package uniquify                   ; Make buffer names unique
  :config (setq uniquify-buffer-name-style
                ;; 'forward
                'post-forward-angle-brackets
                ))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer)
         ("C-x b" . ibuffer))
  ;; Show VC Status in ibuffer
  :config
  (setq ibuffer-expert t)
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)
          (mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-vc                 ; Group buffers by VC project and status
  :ensure t
  :defer t
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

(use-package golden-ratio               ; Automatically resize windows
  :ensure t
  :init
  (defun toggle-golden-ratio ()
    (interactive)
    (if (bound-and-true-p golden-ratio-mode)
        (progn
          (golden-ratio-mode -1)
          (balance-windows))
      (golden-ratio-mode)
      (golden-ratio)))
  :bind (("C-c t g" . toggle-golden-ratio))
  :config
  (setq golden-ratio-extra-commands '(windmove-up
                                      windmove-down
                                      windmove-left
                                      windmove-right
                                      ace-window
                                      ace-delete-window
                                      ace-select-window
                                      ace-swap-window
                                      ace-maximize-window)
        ;; Exclude a couple of special modes from golden ratio, namely
        ;; Flycheck's error list, calc
        golden-ratio-exclude-modes '(flycheck-error-list-mode
                                     calc-mode
                                     dired-mode
                                     ediff-mode
                                     )
        ;; Exclude a couple of special buffers from golden ratio, namely Helm,
        ;; WhichKey, NeoTree, etc.
        golden-ratio-exclude-buffer-regexp
        `(,(rx bos "*" (any "h" "H") "elm*" eos)
          ,(rx bos "*which-key*" eos)
          ,(rx bos "*NeoTree*" eos)))
  :diminish (golden-ratio-mode . "ⓖ"))

(use-package ibuffer-projectile         ; Group buffers by Projectile project
  :ensure t
  :disabled t
  :defer t
  ;; :commands (ibuffer-projectile-filter)
  ;; :init
  ;; (progn
  ;;   (defun ibuffer-projectile-filter (&optional arg)
  ;;     (ibuffer-projectile-set-filter-groups)
  ;;     (unless (eq ibuffer-sorting-mode 'alphabetic)
  ;;       (ibuffer-do-sort-by-alphabetic)))

  ;;   (add-hook 'ibuffer-hook 'ibuffer-projectile-filter))
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

(use-package windmove                   ; Move between windows with Shift+Arrow
  :bind (("C-c w <left>"  . windmove-left)
         ("C-c w <right>" . windmove-right)
         ("C-c w <up>"    . windmove-up)
         ("C-c w <down>"  . windmove-down))
  :config (windmove-default-keybindings 'shift))

(use-package desktop                    ; Save buffers, windows and frames
  :disabled t
  :init (desktop-save-mode)
  :config
  ;; Save desktops a minute after Emacs was idle.
  (setq desktop-auto-save-timeout 60)

  ;; Don't save Magit and Git related buffers
  (dolist (mode '(magit-mode magit-log-mode))
    (add-to-list 'desktop-modes-not-to-save mode))
  (add-to-list 'desktop-files-not-to-save (rx bos "COMMIT_EDITMSG")))

;;; File handling

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package server                     ; The server of `emacsclient'
  :init (server-mode)
  :diminish (server-buffer-clients . " ⓒ"))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (setq dired-auto-revert-buffer t    ; Revert on re-visiting
        ;; Better dired flags: `-l' is mandatory, `-a' shows all files, `-h'
        ;; uses human-readable sizes, and `-F' appends file-type classifiers
        ;; to file names (for better highlighting)
        dired-listing-switches "-alhF"
        dired-ls-F-marks-symlinks t   ; -F marks links with @
        ;; Inhibit prompts for simple recursive operations
        dired-recursive-copies 'always
        ;; Auto-copy to other Dired split window
        dired-dwim-target t)

  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
    ;; `--group-directories-first' lists directories before files, and `-v'
    ;; sorts numbers in file names naturally, i.e. "image1" goes before
    ;; "image02"
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first -v"))))

(use-package dired-x                    ; Additional tools for Dired
  :defer nil
  :commands dired-kill-buffer-jump
  :bind (("C-c f j" . dired-jump)
         ("C-x C-j" . dired-jump)
         ("C-x C-j" . dired-jump)
         ("C-A-j"       . dired-jump)
         ("C-A-x C-A-k" . dired-jump-kill-buffer)
         ("C-A-k"       . dired-jump-kill-buffer)
         ("C-x C-j"     . dired-jump))
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :after dired
  :config
  (setq dired-omit-verbose nil)        ; Shut up, dired

  (defun dired-jump-kill-buffer (&rest)
    (interactive)
    (let ((buf (current-buffer)))
      (dired-jump)
      (kill-buffer buf)))

  (when (eq system-type 'darwin)
    ;; OS X bsdtar is mostly compatible with GNU Tar
    (setq dired-guess-shell-gnutar "tar"))

  ;; Diminish dired-omit-mode. We need this hack, because Dired Omit Mode has
  ;; a very peculiar way of registering its lighter explicitly in
  ;; `dired-omit-startup'.  We can't just use `:diminish' because the lighter
  ;; isn't there yet after dired-omit-mode is loaded.
  (add-function :after (symbol-function 'dired-omit-startup)
                (lambda () (diminish 'dired-omit-mode " ⓞ"))
                '((name . dired-omit-mode-diminish))))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config
  ;; Ignore some additional directories and file extensions
  (dolist (name '(".cask"
                  ".vagrant"
                  ".ensime_cache" ".ensime"
                  ".stack-work"))
    ;; Ignore some additional directories
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (dolist (ext '(".fls" ".out" ; LaTeX
                 ))
    (add-to-list 'ignoramus-file-endings ext))

  (ignoramus-setup))

(use-package hardhat                    ; Protect user-writable files
  :ensure t
  :init (global-hardhat-mode)
  :config (setq hardhat-mode-lighter " Ⓗ"))

(use-package bookmark                   ; Bookmarks for Emacs buffers
  :bind (("C-c f b" . list-bookmarks))
  ;; Save bookmarks immediately after a bookmark was added
  :config (setq bookmark-save-flag 1
                bookmark-default-file (expand-file-name "bookmarks" user-cache-directory)))

(use-package saveplace                  ; Save point position in files
  :init (save-place-mode 1)
  :config
  (progn
    (setq-default save-place t)
    (setq-default save-place-file (expand-file-name "places" user-cache-directory))))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil         ; Shut up, please!
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t)

  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil))
  :diminish (auto-revert-mode . " Ⓐ"))

(use-package visual-regexp              ; Regexp replace with in-buffer display
  :ensure t
  :bind (("C-c s r" . vr/query-replace)
         ("C-c s R" . vr/replace))
)


;;; Basic editing
;; Disable tabs, but give them proper width
;; (setq-default indent-tabs-mode nil
;;               tab-width 4)
;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(diminish 'auto-fill-function " Ⓕ")

(use-package delsel                     ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish (whitespace-cleanup-mode . " Ⓦ"))

(use-package zop-to-char                ; Better zapping
  :disabled t
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package undo-tree                  ; Branching undo
  :disabled t
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

;; (use-package smartparens
;;   :init
;;   (bind-key "C-M-f" #'sp-forward-sexp smartparens-mode-map)
;;   (bind-key "C-M-b" #'sp-backward-sexp smartparens-mode-map)
;;   (bind-key "C-)" #'sp-forward-slurp-sexp smartparens-mode-map)
;;   (bind-key "C-(" #'sp-backward-slurp-sexp smartparens-mode-map)
;;   (bind-key "M-)" #'sp-forward-barf-sexp smartparens-mode-map)
;;   (bind-key "M-(" #'sp-backward-barf-sexp smartparens-mode-map)
;;   (bind-key "C-S-s" #'sp-splice-sexp)
;;   (bind-key "C-M-<backspace>" #'backward-kill-sexp)
;;   (bind-key "C-M-S-<SPC>" (lambda () (interactive) (mark-sexp -1)))

;;   :config
;;   (smartparens-global-mode t)

;;   (sp-pair "'" nil :actions :rem)
;;   (sp-pair "`" nil :actions :rem)
;;   (setq sp-highlight-pair-overlay nil))

; Paired delimiters
(use-package smartparens
  :diminish smartparens-mode
  :init
  (progn
    (setq sp-base-key-bindings 'smartparens)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol t)
    (sp-use-paredit-bindings)
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (setq sp-autoescape-string-quote nil)
    (setq sp-autoinsert-if-followed-by-word t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))
  :bind
  (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package hl-todo                    ; Highlight TODOs in buffers
  :ensure t
  :defer t
  :init (global-hl-todo-mode))

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (progn
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-complete-lisp-symbol-without-namespace))))

(use-package yasnippet                  ; Snippets
  :ensure t
  :defer t
  :diminish (yas-minor-mode . " Ⓨ"))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :diminish company-mode
  :init (global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t)
  :diminish company-mode)

(use-package company-quickhelp          ; Show help in tooltip
  :disabled t                           ; M-h clashes with mark-paragraph
  :ensure t
  :after company
  :config (company-quickhelp-mode))

(use-package company-statistics         ; Sort company candidates by statistics
  :ensure t
  :after company
  :config (company-statistics-mode))

(use-package company-math               ; Completion for Math symbols
  :ensure t
  :after company
  :config
  ;; Add backends for math characters
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package company-emoji              ; Emojis completion like Github/Slack
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-emoji))

(use-package helm-company               ; Helm frontend for company
  :ensure t
  :defer t
  :bind (:map company-mode-map
         ([remap complete-symbol] . helm-company)
         ([remap completion-at-point] . helm-company)
         :map company-active-map
         ("C-:" . helm-company)))

(use-package yaml-mode                  ; YAML
  :ensure t
  :defer t
  :mode ("\\.yml$" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq-local paragraph-separate ".*>-$\\|[   ]*$")
              (setq-local paragraph-start paragraph-separate)
              (run-hooks 'prog-mode-hook))))

(use-package json-mode                  ; JSON files
  :ensure t
  :defer t
  :config
  (progn                                ; https://github.com/skeeto/.emacs.d
    (setf json-reformat:pretty-string? t
          json-reformat:indent-width 2)
    (define-key json-mode-map (kbd "M-q")
      (lambda ()
        (interactive)
        (if (region-active-p)
            (call-interactively #'json-reformat-region)
          (json-reformat-region (point-min) (point-max)))))

    (add-hook 'json-mode-hook
              ;; Fix JSON mode indentation
              (lambda () (setq-local js-indent-level 4)))))

(use-package json-reformat              ; Reformat JSON
  :ensure t
  :defer t
  :bind (("C-c x j" . json-reformat-region)))

(use-package eldoc                      ; Documentation in minibuffer
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)
  :config
  (setq-default eldoc-documentation-function #'describe-char-eldoc)
  :diminish (eldoc-mode . " ⓓ"))

(use-package emacs-lisp-mode
  :bind (("M-." . find-function-at-point))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "el")))
    (bind-key "C-<return>" 'eval-region-or-last-sexp emacs-lisp-mode-map)
    (use-package macrostep
      :bind (("C-c e" . macrostep-expand)
             ("C-A-e" . macrostep-expand))
      :config
      (progn
        (bind-keys
         :map macrostep-keymap
         ("j" . macrostep-next-macro)
         ("k" . macrostep-prev-macro)
         ("C-A-n" . macrostep-next-macro)
         ("C-A-p" . macrostep-prev-macro)
         )))
    (use-package ert
      :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords))))

(use-package etags                      ; Tag navigation
  :defer t
  :config
  ;; Do not query before reverting TAGS tables
  (setq tags-revert-without-query t))

(use-package restclient                 ; ReST REPL for Emacs
  :ensure t
  :defer t)

(use-package company-restclient         ; Company support for restclient
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-restclient))



;;; Emacs Lisp
(bind-key "C-c t d" #'toggle-debug-on-error)

(use-package elisp-slime-nav            ; Jump to definition of symbol at point
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :bind (:map elisp-slime-nav-mode-map
              ("C-c h ." . elisp-slive-nav-describe-elisp-thing-at-point))
  :config
  (dolist (key '("C-c C-d d" "C-c C-d C-d"))
    (define-key elisp-slime-nav-mode-map (kbd key) nil))
  :diminish elisp-slime-nav-mode)

(use-package pcre2el                    ; Convert regexps to RX and back
  :disabled t
  :ensure t
  :init (rxt-global-mode))

(use-package ielm                       ; Emacs Lisp REPL
  :bind (("C-c a '" . ielm)
         ("C-c :"   . ielm))
  :config                               ; https://github.com/skeeto/.emacs.d
  (progn
    (define-key ielm-map (kbd "C-c C-z") #'quit-window)
    (defadvice ielm-eval-input (after ielm-paredit activate)
      "Begin each ielm prompt with a paredit pair."
      (paredit-open-round))
    (defun my-ielm-return ()
      (interactive)
      (let ((end-of-sexp (save-excursion
                           (goto-char (point-max))
                           (skip-chars-backward " \t\n\r")
                           (point))))
        (if (>= (point) end-of-sexp)
            (progn
              (goto-char (point-max))
              (skip-chars-backward " \t\n\r")
              (delete-region (point) (point-max))
              (call-interactively #'ielm-return))
          (call-interactively #'paredit-newline))))

    (defun my-ielm-return-again ()
      (interactive)
      (let ((p (point))
            (end-of-sexp (save-excursion
                           (goto-char (point-max))
                           (skip-chars-backward " \t\n\r")
                           (point))))
        (goto-char (point-max))
        (skip-chars-backward " \t\n\r")
        (delete-region (point) (point-max))
        (call-interactively #'ielm-return)
        (comint-previous-input 1)
        (backward-char (- end-of-sexp p))
        (comint-kill-region (point-min) (+ end-of-sexp 1))))

    (add-hook 'ielm-mode-hook
              (function
               (lambda ()
                 (bind-key "C-<return>" 'my-ielm-return ielm-map)))
              t)
    (add-hook 'ielm-mode-hook
              (function
               (lambda ()
                 (bind-key "<return>" 'my-ielm-return-again ielm-map)))
              t)))

(use-package elisp-mode                 ; Emacs Lisp editing
  :defer t
  :interpreter ("emacs" . emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c m e r" . eval-region)
              ("C-c m e b" . eval-buffer)
              ("C-c m e e" . eval-last-sexp)
              ("C-c m e f" . eval-defun)))

(use-package helm-elisp                 ; Helm commands for elisp
  :ensure helm
  :defer t
  :bind (([remap apropos-command] . helm-apropos)
         ("C-c f l" . helm-locate-library)))


;; ;;; Scala

;; (use-package scala-mode                 ; Scala editing
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq scala-indent:default-run-on-strategy
;;         scala-indent:operator-strategy)

;;   (defun newline-and-indent-with-asterisk ()
;;     (interactive)
;;     (newline-and-indent)
;;     (scala-indent:insert-asterisk-on-multiline-comment))

;;   (define-key scala-mode-map (kbd "RET")
;;     #'newline-and-indent-with-asterisk))

;; (use-package sbt-mode                   ; Scala build tool
;;   :ensure t
;;   :defer t
;;   :bind (:map scala-mode-map
;;               ("C-c m b c" . sbt-command)
;;               ("C-c m b r" . sbt-run-previous-command))
;;   :config
;;   ;; Do not pop up SBT buffers automatically
;;   (setq sbt:display-command-buffer nil)

;;   (defun scala-pop-to-sbt (new-frame)
;;     "Open SBT REPL for this project, optionally in a NEW-FRAME.
;; Select the SBT REPL for the current project in a new window.  If
;; the REPL is not yet running, start it.  With prefix arg, select
;; the REPL in a new frame instead."
;;     (interactive "P")
;;     ;; Start SBT when no running, taken from `sbt:command'
;;     (when (not (comint-check-proc (sbt:buffer-name)))
;;       (sbt:run-sbt))

;;     (let ((display-buffer-overriding-action
;;            (if new-frame '(display-buffer-pop-up-frame) nil)))
;;       (pop-to-buffer (sbt:buffer-name))))

;;   (with-eval-after-load 'scala-mode
;;     (bind-key "C-c m s" #'scala-pop-to-sbt scala-mode-map))

;;   ;; Disable Smartparens Mode in SBT buffers, because it frequently
;;   ;; hangs while trying to find matching delimiters
;;   (add-hook 'sbt-mode-hook
;;             (lambda ()
;;               (when (fboundp 'smartparens-mode)
;;                 (smartparens-mode -1)))))

;; (use-package ensime                     ; Scala interaction mode
;;   :ensure t
;;   :after scala-mode
;;   :bind (:map ensime-mode-map
;;               ("C-c m E" . ensime-reload)
;;               ;; Free M-n and M-p again
;;               ("M-n" . nil)
;;               ("M-p" . nil)
;;               ("<f5>" . ensime-sbt-do-compile)
;;          :map scala-mode-map ("C-c m e" . ensime))
;;   :config
;;   ;; ;; Enable Ensime for all Scala buffers.
;;   (add-hook 'scala-mode-hook #'ensime-mode)

;;   ;; Compile on save.  My projects are small enough :)
;;   (setq ensime-sbt-perform-on-save "test:compile"))

;; (use-package ensime-expand-region       ; Integrate Ensime into expand-region
;;   :ensure ensime
;;   :after ensime)

;; (use-package play-routes-mode           ; Mode for Play 2 routes files
;;   :ensure t
;;   :defer t)

;; (use-package flycheck-ensime            ; Ensime-based checker for Flycheck
;;   :disabled t
;;   :load-path "lisp/"
;;   :defer t)


;;; Haskell
(use-package haskell-mode               ; Haskell major mode
  :ensure t
  :defer t
  :bind (:map haskell-mode-map
              ("M-." . haskell-mode-jump-to-def-or-tag)
              ("C-c m i j" . haskell-navigate-imports)
              ("C-c m i s" . haskell-sort-imports)
              ("C-c m i a" . haskell-align-imports)
              ;; Recommended Haskell Mode bindings, see
              ;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html
            )
  :config
  (setq haskell-tags-on-save t          ; Regenerate TAGS on save
        haskell-process-log t           ; Show log for GHCI process
        ;; Remove unused imports and auto-import modules
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t)

  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode) ; IMenu support
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(use-package haskell                    ; Interactive Haskell
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-mode-map
         ("C-c C-l" . haskell-process-load-file)
         ("C-`" . haskell-interactive-bring)
         ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c c" . haskell-process-cabal)
         :map interactive-haskell-mode-map
         ("C-c m t" . haskell-mode-show-type-at))
  :init (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package haskell-compile            ; Haskell compilation
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-mode-map
              ("C-c m c" . haskell-compile)
              ("<f5>" . haskell-compile))
  :config
  ;; Build with Stack
  (setq haskell-compile-cabal-build-command "stack build"))

(use-package cabal-mode                 ; Cabal files
  :ensure haskell-mode
  :defer t
  :bind (:map haskell-cabal-mode-map
              ("C-`" . haskell-interactive-bring)
              ("C-c C-k" . haskell-interactive-mode-clear)
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c c" . haskell-process-cabal)))

(use-package hindent                    ; Haskell indentation
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook #'hindent-mode)
  :config
  (setq hindent-style "gibiansky"))



;;; Project management with Projectile
;; http://writequit.org/org/settings.html
;; http://joelmccracken.github.io/entries/project-local-variables-in-projectile-with-dirlocals/
(use-package projectile                 ; Project management for Emacs
  :ensure t
  :bind (([remap compile] . projectile-compile-project))
  :init (projectile-global-mode)
  :config
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (setq projectile-completion-system 'helm
        projectile-find-dir-includes-top-level t)

  (defun neotree-project-root (&optional directory)
    "Open a NeoTree browser for a project DIRECTORY."
    (interactive)
    (let ((default-directory (or directory default-directory)))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (neotree-find (projectile-project-root)))))

  (progn
    (bind-keys
     :map projectile-mode-map
     ("C-x C-f" . helm-find-files)
     ("C-x C-p" . projectile-find-file)
     ("C-c p d" . projectile-dired)
     ("C-c p D" . projectile-find-dir))
    (setq projectile-known-projects-file (expand-file-name  "projectile-bookmarks.eld" user-cache-directory)
          projectile-cache-file (expand-file-name  "projectile.cache" user-cache-directory))
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ido)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")

    (use-package cc-mode
      :defer t
      :config (bind-key "C-c C-c" 'projectile-compile-project java-mode-map))
    (use-package make-mode
      :defer t
      :config (bind-key "C-c C-c" 'projectile-compile-project makefile-mode-map)))

  :diminish projectile-mode)

(use-package helm-projectile            ; Helm frontend for Projectile
  :defer t
  :ensure t
  :after projectile
  :bind (([remap projectile-find-file] . helm-projectile-find-file)
         ("C-c s p" . helm-projectile-ag)
         :map helm-projectile-projects-map
              ("C-t" . neotree-project-root))
  :config
  (helm-projectile-on)

  (setq projectile-switch-project-action #'helm-projectile)

  (helm-add-action-to-source "Open NeoTree `C-t'"
                             #'neotree-project-root
                             helm-source-projectile-projects 1))





;;; OTHER?
(use-package paredit
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
    (add-hook 'lisp-mode-hook #'paredit-mode)
    (add-hook 'scheme-mode-hook #'paredit-mode)
    (add-hook 'ielm-mode-hook #'paredit-mode)
    (add-hook 'clojure-mode-hook #'paredit-mode))
  :config (define-key paredit-mode-map (kbd "C-j") #'join-line))
 
(use-package paren
  :config (show-paren-mode))

;; No, thank you, I don't need the Git integration
(setq vc-handled-backends nil)





;; ;; Install extensions if they're missing
;; (defun init--install-packages ()
;;   (packages-install
;;    '(use-package
;;       racket-mode
;;       dash
;;       highlight-escape-sequences
;;       whitespace-cleanup-mode
;;       elisp-slime-nav
;;       smooth-scrolling
;;       shell-command
;;       easy-kill
;;       rainbow-mode
;;       diminish
;;       smartparens
;;       eval-sexp-fu
;;       solarized-theme)))

;; (init--install-packages)

;; (eval-when-compile
;;   (require 'use-package))
;; (require 'diminish)                ;; if you use :diminish
;; (require 'bind-key)                ;; if you use any :bind variant
;; (require 'dash)
;; (require 'rx)

;; (eval-after-load "dash" '(dash-enable-font-lock))

;; (use-package f
;;   :ensure t)

;; (use-package s
;;   :ensure t)

;; (use-package undo-tree
;;   :ensure t
;;   :init (global-undo-tree-mode)
;;   :diminish undo-tree-mode
;;   :bind (("C-u" . undo-tree-undo)
;;          ("C-S-u" . undo-tree-redo)
;;          ("M-u" . undo-tree-visualize))
;;   :config (bind-keys :map undo-tree-map
;;                      ("C-/" . nil)
;;                      ("C-?" . nil)
;;                      ("C-_" . nil)))

;; ;(require 'appearance)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;                           ;;
;; ;;          SETTINGS         ;;
;; ;;                           ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; ;; Write backup files to own directory
;; (setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
;;       auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
;;       ;; Make backups of files, even when they're in version control
;;       vc-make-backup-files t)

;; ;; Save point position between sessions
;; (use-package saveplace
;;   :init
;;   (setq save-place-file (expand-file-name ".places" user-emacs-directory))
;;   (setq-default save-place t))

;; (setq view-read-only t)

;; (require 'sane-defaults)

;; ;; Setup environment variables from the user's shell.
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :if (and (eq system-type 'darwin) (display-graphic-p))
;;   :config
;;   (exec-path-from-shell-initialize))

;; (define-minor-mode ze-mode
;;   "Ze mode to override other bindings."
;;   :init-value nil
;;   :lighter " ze"
;;   :keymap (make-sparse-keymap "ze"))

;; (bind-keys :prefix-map ze-prefix
;;            :prefix "<f3>"
;;            :prefix-docstring
;;            "Prefix for counsel / buffers / filesystem / windows-layout commands")

;; ;(eval-after-load 'shell '(require 'setup-shell))
;; (require 'setup-hippie)

;; (use-package yasnippet
;;   :ensure t
;;   :bind (("C-x t" . yas-expand))
;;   :init
;;   (progn
;;     (setq yas-verbosity 1
;;           yas-snippet-dirs `(,(expand-file-name "snippets" user-emacs-directory))
;;           yas-wrap-around-region t)

;;     (yas-global-mode 1))
;;   :config
;;   (progn
;;     ;; Inter-field navigation
;;     (defun yas/goto-end-of-active-field ()
;;       (interactive)
;;       (let* ((snippet (car (yas--snippets-at-point)))
;;              (position (yas--field-end (yas--snippet-active-field snippet))))
;;         (if (= (point) position)
;;             (move-end-of-line 1)
;;           (goto-char position))))

;;     (defun yas/goto-start-of-active-field ()
;;       (interactive)
;;       (let* ((snippet (car (yas--snippets-at-point)))
;;              (position (yas--field-start (yas--snippet-active-field snippet))))
;;         (if (= (point) position)
;;             (move-beginning-of-line 1)
;;           (goto-char position))))

;;     (bind-keys :map yas-keymap
;;                ("C-e" . yas/goto-end-of-active-field)
;;                ("C-a" . yas/goto-start-of-active-field)
;;                ("<return>" . yas/exit-all-snippets)
;;                :map yas-minor-mode-map
;;                ("<tab>" . nil)
;;                ("TAB" . nil)))
;;   :diminish yas-minor-mode)

;; (use-package datomic-snippets
;;   :ensure t)

;; (semantic-mode 1)

;; (use-package ivy
;;   :ensure t
;;   :init (ivy-mode 1)
;;   :bind (
;;          :map ze-prefix
;;          ("r" . ivy-resume)
;;          ("b" . ivy-switch-buffer)
;;          ;; TODO this one doesn't appear to respect persp
;;          ("B" . ivy-switch-buffer-other-window)
;;          :map ivy-minibuffer-map
;;          ("M-c" . ivy-kill-ring-save))
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   :diminish ivy-mode)

;; (use-package ivy-hydra
;;   :ensure t
;;   :after ivy)

;; (use-package swiper
;;   :ensure t
;;   :bind ())

;; (use-package "isearch"
;;   ;; Defer because `isearch' is not a feature and we don't want to `require' it
;;   :defer t
;;   :init
;;   ;; `:diminish' doesn't work for isearch, because it uses eval-after-load on
;;   ;; the feature name, but isearch.el does not provide any feature.  For the
;;   ;; same reason we have to use `:init', but isearch is always loaded anyways.
;;   (diminish 'isearch-mode)
;;   (setq isearch-allow-scroll t)
;;   (bind-keys :map isearch-mode-map
;;              ("<escape>" . isearch-abort)
;;              ("C-q" . isearch-abort)))

;; (use-package projectile
;;   :ensure t
;;   :init (projectile-global-mode)
;;   :bind (
;;          :map ze-prefix
;;          ("d" . projectile-find-dir))
;;   :config
;;   ;; Remove dead projects when Emacs is idle
;;   (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

;;   (setq projectile-completion-system 'ivy
;;         projectile-find-dir-includes-top-level t
;;         projectile-indexing-method 'alien
;;         projectile-enable-caching nil)
;;   (add-to-list 'projectile-globally-ignored-directories "elpa")
;;   (add-to-list 'projectile-globally-ignored-directories ".node_modules")
;;   (add-to-list 'projectile-globally-ignored-directories ".m2")
;;   (setq projectile-switch-project-action
;;         (lambda ()
;;           (dired (projectile-project-root))))

;;   :diminish projectile-mode)

;; (setq programming-modes
;;       '(clojure-mode emacs-lisp-mode racket-mode))

;; ;; map files to modes
;; (require 'mode-mappings)

;; ;; highlight escape sequences, works only in javascript
;; (use-package highlight-escape-sequences
;;   :ensure t
;;   :init (hes-mode))

;; ;; Functions (load all files in defuns-dir)
;; (setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
;; (dolist (file (directory-files defuns-dir t "\\w+"))
;;   (when (file-regular-p file)
;;     (load file)))

;; ;; Enable comment annotation keywords in programming modes
;; (comment-annotations-in-modes programming-modes)

;; (use-package expand-region
;;   :bind (("M-SPC" . er/expand-region))
;;   :ensure t
;;   :config
;;   (setq expand-region-contract-fast-key "-"
;;         expand-region-reset-fast-key "="))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;                            ;;
;; ;; STOPPED READING AFTER THIS ;;
;; ;;                            ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Elisp go-to-definition with M-. and back again with M-,
;; (autoload 'elisp-slime-nav-mode "elisp-slime-nav")
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (elisp-slime-nav-mode t)
;;             (eldoc-mode 1)
;;             (rainbow-mode +1)))

;; (require 'whitespace)
;; (setq whitespace-line-column 80) ;; limit line length
;; (setq whitespace-style '(face tabs empty trailing lines-tail))

;; (require 'smartparens)
;; ;(require 'setup-smartparens)
;; (require 'smartparens-config)
;; (show-smartparens-global-mode)
;; (use-package smartparens
;;   :init
;;   (bind-key "C-M-f" #'sp-forward-sexp smartparens-mode-map)
;;   (bind-key "C-M-b" #'sp-backward-sexp smartparens-mode-map)
;;   (bind-key "C-)" #'sp-forward-slurp-sexp smartparens-mode-map)
;;   (bind-key "C-(" #'sp-backward-slurp-sexp smartparens-mode-map)
;;   (bind-key "M-)" #'sp-forward-barf-sexp smartparens-mode-map)
;;   (bind-key "M-(" #'sp-backward-barf-sexp smartparens-mode-map)
;;   (bind-key "C-S-s" #'sp-splice-sexp)
;;   (bind-key "C-M-<backspace>" #'backward-kill-sexp)
;;   (bind-key "C-M-S-<SPC>" (lambda () (interactive) (mark-sexp -1)))

;;   :config
;;   (smartparens-global-mode t)

;;   (sp-pair "'" nil :actions :rem)
;;   (sp-pair "`" nil :actions :rem)
;;   (setq sp-highlight-pair-overlay nil))

;; ;; Emacs server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; (use-package recentf
;;   :init (recentf-mode)
;;   :config
;;   (setq recentf-max-saved-items 200
;;         recentf-max-menu-items 15
;;         ;; Cleanup recent files only when Emacs is idle, but not when the mode
;;         ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
;;         ;; idles often enough to have the recent files list clean up regularly
;;         recentf-auto-cleanup 300
;;         recentf-exclude (list "/\\.git/.*\\'"
;;                               "/elpa/.*\\'"
;;                               "/\\.node_modules/.*\\'"
;;                               ;; #'ignoramus-boring-p
;;                               )))

;; ;; revert buffers automatically when underlying files are changed externally
;; (use-package autorevert
;;   :init (global-auto-revert-mode t)
;;   :config
;;   (setq auto-revert-verbose nil
;;         ;; Revert Dired buffers, too
;;         global-auto-revert-non-file-buffers t)
;;   (when (eq system-type 'darwin)
;;     ;; File notifications aren't supported on OS X
;;     (setq auto-revert-use-notify nil))
;;   :diminish auto-revert-mode)

;; (use-package re-builder
;;   :defer t
;;   :config (setq reb-re-syntax 'rx))

;; ;; Elisp
;; (use-package elisp-mode
;;   :interpreter ("emacs" . emacs-lisp-mode)
;;   :bind (:map emacs-lisp-mode-map
;;               ("C-c C-c" . eval-defun)
;;               ("C-c C-e" . eval-last-sexp)
;;               ("C-c C-k" . eval-buffer)))

;; (use-package macrostep
;;   :ensure t
;;   :after elisp-mode
;;   :bind (:map emacs-lisp-mode-map ("C-c m" . macrostep-expand)
;;          :map lisp-interaction-mode-map ("C-c m" . macrostep-expand)))

;; (use-package golden-ratio
;;   :ensure t
;;   :init
;;   (defun ze-toggle-golden-ratio ()
;;     (interactive)
;;     (if (bound-and-true-p golden-ratio-mode)
;;         (progn
;;           (golden-ratio-mode -1)
;;           (balance-windows))
;;       (golden-ratio-mode)
;;       (golden-ratio)))
;;   :diminish golden-ratio-mode
;;   :config
;;   (progn
;;     (setq golden-ratio-exclude-modes '("bs-mode"
;;                                        "calc-mode"
;;                                        "ediff-mode"
;;                                        "gud-mode"
;;                                        "gdb-locals-mode"
;;                                        "gdb-registers-mode"
;;                                        "gdb-breakpoints-mode"
;;                                        "gdb-threads-mode"
;;                                        "gdb-frames-mode"
;;                                        "gdb-inferior-io-mode"
;;                                        "gud-mode"
;;                                        "gdb-inferior-io-mode"
;;                                        "gdb-disassembly-mode"
;;                                        "gdb-memory-mode"
;;                                        "restclient-mode"
;;                                        "speedbar-mode"
;;                                        ))

;;     (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")

;;     (setq golden-ratio-extra-commands
;;           (append golden-ratio-extra-commands
;;                   '(ace-window
;;                     ace-delete-window
;;                     ace-select-window
;;                     ace-swap-window
;;                     ace-maximize-window
;;                     avy-pop-mark
;;                     windmove-left
;;                     windmove-right
;;                     windmove-up
;;                     windmove-down
;;                     select-window-0
;;                     select-window-1
;;                     select-window-2
;;                     select-window-3
;;                     select-window-4
;;                     select-window-5
;;                     select-window-6
;;                     select-window-7
;;                     select-window-8
;;                     select-window-9
;;                     buf-move-left
;;                     buf-move-right
;;                     buf-move-up
;;                     buf-move-down
;;                     ess-eval-buffer-and-go
;;                     ess-eval-function-and-go
;;                     ess-eval-line-and-go
;;                     other-window
;;                     ze-other-window
;;                     quit-window)))))

;; ;; clojure
;; (require 'setup-clj)

;; ;; racket
;; (use-package racket-mode
;;   ;; :ensure t
;;   :load-path "site-lisp/racket-mode/"
;;   :mode (("\\.rkt\\'" . racket-mode))
;;   :config
;;   (add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))
;;   ;; :config (progn
;;   ;;           (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
;;   ;;           (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable))

;;   ;; (lookup-key racket-mode-map (kbd "C-c C-e"))
;;   (bind-keys :map racket-mode-map
;;              ;; ("C-c m" . racket-macro-expand-map)
;;              ("C-c C-c" . racket-send-definition)
;;              ("C-c C-e" . racket-send-last-sexp)))







