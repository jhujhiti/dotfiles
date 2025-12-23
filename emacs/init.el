;; -*- lexical-binding: t; flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-

(defun add-to-path (dir)
  "Add DIR to the PATH environment variable and exec-path."
  (add-to-list 'exec-path dir)
  (let ((pattern (concat "\\(^\\|:\\)" (regexp-quote dir) "\\($\\|:\\)"))
        (path (getenv "PATH")))
    (when (not (string-match pattern path))
      (setenv "PATH" (concat dir ":" path)))))

(dolist (dir '("/usr/local/bin"
               "~/.nix-profile/bin"
               "/nix/var/nix/profiles/default/bin"
               "/run/current-system/sw/bin"
               "~/.cargo/bin"
               "~/go/bin"
               "~/.pyenv/shims"
               "~/.krew/bin"
               "~/bin"))
  (when (file-directory-p dir)
    (add-to-path dir)))

(defvar bootstrap-version)
(let ((install-file
       (expand-file-name "straight-install.el" user-emacs-directory))
      (bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (load install-file nil 'nomessage))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setopt straight-use-package-by-default t)

;; silly but fixes some "is not known to be defined" warnings
(use-package straight
  :functions straight-use-package
  :defines straight-use-package-by-default)

;;; general emacs setup
(use-package diminish
  :functions diminish)

(use-package emacs
  :straight (:type built-in)
  :init
  ;; obviously we want utf-8 everywhere
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (set-language-environment "UTF-8")
  ;; don't make me type "yes" or "no"
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; stop asking to follow symlinks, just do it
  (setq vc-follow-symlinks t)
  ;; make me confirm quitting
  (setq confirm-kill-emacs 'y-or-n-p)
  ;; performance settings recommended somewhere for lsp-mode
  (setq
   ;; big garbage collection threshold
   gc-cons-threshold (* 100 1024 1024)
   ;; increase how much emacs can read from processes
   read-process-output-max (* 1024 1024)
   ;; kill the welcome screen
   inhibit-startup-screen t)
  (if (display-graphic-p)
      ;; settings for the gui
      (progn
        ;; it doesn't take up any screen real estate on mac os
        (when (not (eq window-system 'ns)) (menu-bar-mode -1))
        (tool-bar-mode -1)
        (setq default-frame-alist
              '((width . 132)
                (height . 44)
                (vertical-scroll-bars)))
        (let* (
               ;; hunt for a working font in this order
               (families '("Source Code Pro" "DejaVu Sans Mono"))
               ;; not sure why mac os looks wrong at the same size as linux
               (size (cond
                      ((eq window-system 'ns) 12)
                      (t 9)))
               (fonts (seq-map (apply-partially 'font-spec :size (float size) :family) families))
               (found (seq-find 'find-font fonts)))
          ;; this really should work but font-info ignores the size?
          ;; (when found (add-to-list 'default-frame-alist `(font . ,(aref (font-info found) 1))))))
          ;; we'll do this stupid shit instead, just reconstruct the name string
          (when found (add-to-list
                       'default-frame-alist
                       `(font . ,(concat
                                  (symbol-name (font-get found :family))
                                  "-"
                                  (number-to-string size)))))))
    ;; settings for the console
    (progn
      ;; fix awful modeline colors on the console
      (add-to-list 'face-remapping-alist '(mode-line . ((:background "brightblack" :foreground "brightwhite") mode-line)))
      (add-to-list 'face-remapping-alist '(mode-line-inactive . ((:background "black" :foreground "white") mode-line)))))
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode)
  ;; only prompt for xref identifier when one isn't under the cursor
  (setopt xref-prompt-for-identifier nil)
  ;; treat _ as part of a word
  ;; TODO: why do i need to do this in a hook?
  (add-to-list 'after-change-major-mode-hook
	       (lambda () (modify-syntax-entry ?_ "w")))
  (defconst jhujhiti/c-style
    '("k&r"
      (c-basic-offset . 4)
      (c-offsets-alist . ((innamespace . [0])))))
  (c-add-style "jhujhiti/c-style" jhujhiti/c-style)
  (setq-default
   show-trailing-whitespace t
   indent-tabs-mode nil
   tab-always-indent nil
   indent-line-function 'tab-to-tab-stop
   tab-width 4
   c-basic-offset 4
   c-default-style "jhujhiti/c-style"
   require-final-newline t)
  :hook (text-mode . (lambda () (setq-local indent-line-function 'indent-relative))))
(use-package json
  :straight (:type built-in))
(use-package autorevert
  :straight (:type built-in)
  :diminish auto-revert-mode)
(use-package flyspell
  :straight (:type built-in)
  :diminish
  :hook (text-mode prog-mode)
  :config (setq flyspell-prog-text-faces
                (delq 'font-lock-string-face flyspell-prog-text-faces)))

(use-package evil
  :functions evil-mode
  :init (setq evil-want-keybinding nil
              evil-undo-system 'undo-tree)
  :config (evil-mode 1)
  :bind
  (:map evil-motion-state-map
	("gr" . 'xref-find-references)))
(use-package evil-collection
  :functions evil-collection-init
  :after (magit evil diminish)
  :config
  (evil-collection-init)
  (diminish 'evil-collection-unimpaired-mode))
(use-package evil-numbers
  :after evil)
(use-package evil-quickscope
  :after evil)
(use-package evil-surround
  :functions global-evil-surround-mode
  :after evil
  :config (global-evil-surround-mode 1))
;; leaving out evil-nerd-commenter and evil-leader

(use-package ivy
  :functions ivy-mode
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (global-set-key (kbd "C-c C-r") 'ivy-resume))
(use-package ivy-xref
  :functions ivy-xref-show-defs ivy-xref-show-xrefs
  :after ivy
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))
(use-package flyspell-correct-ivy
  :functions flyspell-correct-ivy flyspell-correct-wrapper
  :demand
  :custom
  (flyspell-correct-interface #'flyspell-correct-ivy)
  :bind (:map flyspell-mode-map ("M-$" . 'flyspell-correct-wrapper)))
(use-package counsel
  :functions counsel-mode
  :diminish
  :after ivy
  :config
  (counsel-mode 1)
  :custom
  (counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions)))
(use-package swiper
  :functions swiper
  :after ivy
  :bind ("C-s" . 'swiper))
(use-package which-key
  :diminish
  :config (which-key-mode))

(use-package company
  :demand
  :diminish
  :after lsp-mode)

(use-package undo-tree
  :functions global-undo-tree-mode
  :diminish
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

(use-package yasnippet
  :functions yas-global-mode
  :diminish yas-minor-mode
  :config (yas-global-mode))

(use-package base16-theme
  :config (load-theme 'base16-eighties t))

;;; general programming
(defmacro my/ts-grammar (lang &optional branch repo)
  `(let* ((pkg-name (intern (concat "my-ts-grammar-" (symbol-name ,lang))))
          (real-repo (cond
                      ((stringp ,repo) '(:type git :host github :repo ,repo))
                      ((null ,repo) `(:type git :host github :repo ,(concat "tree-sitter/tree-sitter-" (symbol-name ,lang))))
                      (t repo))))
     (straight-use-package (append (list pkg-name)
                                   real-repo
                                   (when (stringp ,branch) `(:branch ,branch))
                                   '(:post-build (my/ts-compile-grammar))))))
;; cribbed from https://leba.dev/blog/2022/12/12/(ab)using-straightel-for-easy-tree-sitter-grammar-installations/
(defun my/ts-compile-grammar (&optional path)
  (let* ((destination (expand-file-name "tree-sitter" user-emacs-directory))
         (default-directory (expand-file-name "src/" (or path default-directory)))
         (parse-name
          (thread-last (expand-file-name "grammar.json" default-directory)
                       (json-read-file)
                       (alist-get 'name))))
    (message "Compiling grammar for %s" parse-name)
    (make-directory destination 'parents)
    (with-temp-buffer
      (unless
          (zerop
           (apply #'call-process
                  (if (file-exists-p "scanner.cc") "c++" "cc") nil t nil
                  "parser.c" "-I." "--shared" "-O2" "-o"
                  (expand-file-name
                   (format "libtree-sitter-%s%s" parse-name module-file-suffix)
                   destination)
                  (cond ((file-exists-p "scanner.c") '("scanner.c"))
                        ((file-exists-p "scanner.cc") '("scanner.cc")))))
        (user-error "Unable to compile grammar\n%s" (buffer-string))))))
(use-package treesit
  :straight (:type built-in)
  :config
  ;; TODO: this is global now.
  ;; https://lists.endsoftwarepatents.org/archive/html/emacs-devel/2024-12/msg00286.html
  ;; mentions a per-mode alist instead, but i don't have it. maybe in
  ;; emacs 31?
  (setq treesit-font-lock-level 4))

(use-package transient)
(use-package magit
  :after transient
  :bind ("C-c g" . magit-status))
(use-package magit-todos
  :after magit
  :functions magit-todos-mode
  :config (magit-todos-mode 1))
(use-package forge
  :after magit
  :init
  ;; TODO: figure out why evil-collection is complaining and setting
  ;; this itself
  (setq forge-add-default-bindings nil))
(use-package git-modes)

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  ;; fix up some bogus warnings in use-package init.el stuff
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package apheleia
  :diminish)

;; project navigation
(use-package projectile)
(use-package treemacs
  :disabled
  :custom
  (treemacs-follow-after-init t)
  (treemacs-tag-follow-mode t)
  (treemacs-project-follow-mode t)
  :bind ([f5] . treemacs))
(use-package treemacs-evil
  :disabled
  :after (treemacs evil))

;; lsp
(use-package lsp-mode
  :custom
  (lsp-modeline-diagnostics-enable nil)
  (lsp-keep-workspace-alive nil))
(use-package lsp-ivy
  :after (lsp-mode ivy))
(use-package lsp-treemacs
  :disabled
  :functions lsp-treemacs-sync-mode
  :after (treemacs lsp-mode)
  :config (lsp-treemacs-sync-mode))

;; highlights for TODO/indentation levels/parens
(use-package highlight-indentation
  :hook yaml-ts-mode)
(use-package hl-todo
  :hook (prog-mode LaTeX-mode))
(use-package rainbow-delimiters
  :hook emacs-lisp)

;;; languages
;; python
(use-package python
  :after (lsp-mode treesit)
  :straight (:type built-in)
  :init
  (my/ts-grammar 'python)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :config
  ;; run this early so anything like uv that sets up the virtualenv in
  ;; the hook runs after it (add-hook prepends)
  (add-hook 'python-ts-mode-hook #'lsp))
(use-package pyenv-mode
  :functions pyenv-mode
  :if (executable-find "pyenv")
  :config
  (setenv "WORKON_HOME" "~/.virtualenvs")
  (pyenv-mode 1))
(use-package uv-mode
  :if (executable-find "uv")
  :hook (python-ts-mode . uv-mode-auto-activate-hook))
(use-package jedi-core)
(use-package company-jedi
  :after (jedi-core company)
  :config (add-to-list 'company-backends 'company-jedi))

;; rust
(use-package rust-mode)
(use-package flycheck-rust
  :after (rust-ts-mode flycheck)
  :hook (rust-ts-mode . flycheck-rust-setup))
(use-package rust-ts-mode
  :after (lsp-mode treesit apheleia rust-mode)
  :straight (:type built-in)
  :hook ((rust-ts-mode . apheleia-mode)
         (rust-ts-mode . lsp-mode)
         (rust-ts-mode . lsp-inlay-hints-mode))
  :init
  (my/ts-grammar 'rust)
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  :config
  (setq lsp-rust-server 'rust-analyzer
        lsp-inlay-hint-enable t))

;; go
(use-package go-ts-mode
  :after (lsp-mode treesit)
  :straight (:type built-in)
  :init
  (my/ts-grammar 'go)
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  :hook (go-ts-mode . lsp))
(use-package company-go
  :after company)

;; c/c++
(defun my/c-lsp-setup () (setq-local lsp-enable-indentation nil
                                     lsp-enable-on-type-formatting nil))
(use-package c-ts-mode
  :after (lsp-mode treesit)
  :straight (:type built-in)
  :init
  (my/ts-grammar 'c)
  (my/ts-grammar 'cpp)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  :hook ((c-ts-mode . lsp)
         (c-ts-mode . my/c-lsp-setup)
         (c++-ts-mode . lsp)
         (c++-ts-mode . my/c-lsp-setup)))

;; yaml
(use-package yaml-ts-mode
  :after (lsp-mode treesit)
  :straight (:type built-in)
  :init
  (my/ts-grammar 'yaml nil "tree-sitter-grammars/tree-sitter-yaml")
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
  :config
  (add-hook 'yaml-ts-mode-hook #'lsp))

;; terraform
(use-package terraform-mode
  :after apheleia
  :hook (terraform-mode . apheleia-mode)
  :config (let ((cmd (if (executable-find "tofu") "tofu" "terraform")))
            (setf (alist-get 'terraform apheleia-formatters)
                  `(,cmd "fmt" "-"))))

;; html/css
(use-package web-mode
  :custom (web-mode-markup-indent-offset 2))

;; nix
(use-package nix-mode)
(use-package nix-ts-mode
  :after (treesit nix-mode)
  :init
  (my/ts-grammar 'nix nil "nix-community/tree-sitter-nix")
  (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))

;; tex
(use-package auctex
  :config
  (add-to-list 'safe-local-variable-values '(TeX-command-extra-options . "-shell-escape"))
  (add-to-list 'safe-local-variable-values '(TeX-command-force . "LaTeX")))

;; dockerfiles
(use-package dockerfile-ts-mode
  :after treesit
  :straight (:type built-in)
  :init
  (my/ts-grammar 'dockerfile nil "camdencheek/tree-sitter-dockerfile")
  (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode)))

;; k8s manifests
(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode))

;; jinja
(use-package jinja2-mode)

;; debian packaging modes
(use-package dpkg-dev-el)

;; systemd unit modes
(use-package systemd)

;;; customize junk
;; custom.el will not be in git
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
      (load custom-file))
