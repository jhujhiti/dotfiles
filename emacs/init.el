;;; package --- init.el

; ----- Helper functions -----
(defun make-mode-hooks (&rest modes)
  "Concatenates -mode-hook onto the end of each argument (MODES)
and returns them as interned symbols."
  (mapcar (lambda (it)
            (intern (concat (symbol-name it) "-mode-hook")))
          modes))

(defun apply-mode-hook (hook &rest modes)
  "Apply HOOK to each mode listed in MODES.
MODES is specified as in make-mode-hooks.
Example: (apply-mode-hook 'flymake-mode \"emacs-lisp\" \"haskell\")"
  (mapc (lambda (it)
          (add-hook it hook))
        (apply 'make-mode-hooks modes)))

(defun in-comment-p (&optional point)
  "Returns t if POINT or the cursor is inside a comment. nil otherwise."
  (not (null (nth 4 (syntax-ppss point)))))

(defun in-string-p (&optional point)
  "Returns t if POINT or the cursor is inside a string. nil otherwise."
  (not (null (nth 3 (syntax-ppss point)))))

; these are doing subtly different things (add-to-list appends, setenv
; appends and prepends depending on how it's used (obviously). this is
; probably going to bite me some day but for now, i don't care.
(when (string-equal system-type "darwin")
  (add-to-list 'exec-path "/usr/local/bin")
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (when (file-executable-p "/nix/var/nix/profiles/default/bin/nix-shell")
    (add-to-list 'exec-path "~/.nix-profile/bin")
    (add-to-list 'exec-path "/nix/var/nix/profiles/default/bin")
    (add-to-list 'exec-path "/run/current-system/sw/bin")
    (add-to-list 'exec-path "/nix/var/nix/profiles/default/bin")
    (setenv "PATH" (concat "~/.nix-profile/bin:/nix/var/nix/profiles/default/bin:" (getenv "PATH") ":/run/current-system/sw/bin:/nix/var/profiles/default/bin"))))


; ----- Package bootstrap -----
(require 'package)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package base16-theme)
(use-package company
  :init
  (setq lsp-completion-provider :capf))
(use-package company-go)
(use-package company-jedi)
(use-package company-shell)
(use-package counsel :after ivy)
(use-package diminish)
(use-package dockerfile-mode)
(use-package elpy)
(use-package evil
  :init (progn
          (setq evil-want-keybinding nil)
          (setq evil-undo-system 'undo-tree))
  :config (evil-mode 1))
(use-package evil-leader)
(use-package evil-collection
  :after (evil magit)
  :config (evil-collection-init))
(use-package evil-numbers)
(use-package evil-quickscope)
(use-package evil-surround)
(use-package flymake)
(use-package flymake-css)
;; Causes errors, not interested in why right now
;; (use-package flymake-cursor)
(use-package flymake-haskell-multi)
(use-package flymake-json)
;(use-package flymake-python-pyflakes)
(use-package flymake-ruby)
(use-package flymake-shell)
(use-package flyspell-correct-ivy)
(use-package forge :after magit)
(use-package git-modes)
;; (use-package ghc)
;; (use-package ghc-imported-from)
(use-package go-mode)
(use-package graphviz-dot-mode)
(use-package haskell-mode)
(use-package inf-ruby)
(use-package ivy)
(use-package jedi)
(use-package jedi-core)
(use-package jinja2-mode)
(use-package kubernetes)
(use-package kubernetes-evil :after kubernetes)
(use-package lsp-ivy :after lsp-mode)
(use-package lsp-mode :config (setq lsp-modeline-diagnostics-enable nil))
(use-package markdown-mode)
(use-package magit)
(use-package nasm-mode)
(use-package nix-mode)
(use-package ox-hugo :after ox)
(use-package pandoc)
(use-package pandoc-mode)
(use-package pyenv-mode
  :if (executable-find "pyenv")
  :config
  (setenv "WORKON_HOME" "~/.virtualenvs")
  (pyenv-mode t))
(use-package rainbow-delimiters)
(use-package rubocop)
(use-package salt-mode)
(use-package swiper :after ivy)
(use-package systemd)
(use-package terraform-mode)
(use-package which-key)
(use-package undo-tree
  :config (global-undo-tree-mode))

(setq-local my-lisp-path (concat user-emacs-directory "lisp"))
(setq-local my-site-lisp-path (concat user-emacs-directory "site-lisp"))
(byte-recompile-directory my-lisp-path 0)
(add-to-list 'load-path my-lisp-path)
(require 'junos-mode)

(if (file-directory-p my-site-lisp-path)
    (progn
      (byte-recompile-directory  my-site-lisp-path 0)
      (add-to-list 'load-path my-site-lisp-path)
      (require 'site-lisp)))

(use-package evil-surround :config (global-evil-surround-mode 1))
(setq evil-highlight-closing-paren-at-point-states '(not emacs insert replace normal visual))

; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(set-language-environment "UTF-8")

; completion
(apply-mode-hook 'company-mode 'prog)
(diminish 'company-mode)

; syntax/style
(apply-mode-hook 'flymake-mode 'prog)
;; show trailing whitespace
(setq-default show-trailing-whitespace t)
;; tabs and indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-default-style "k&r")
(apply-mode-hook
 '(setq indent-line-function 'tab-to-tab-stop)
 'fundamental 'conf)
;; always append a newline at the end of files when saving
(setq require-final-newline t)

; beautification
;; basic look-and-feel
; default height and width
(add-to-list 'default-frame-alist '(width . 132))
(add-to-list 'default-frame-alist '(height . 44))
; Mac OS (ns) DPI scaling is a pain. or at least that's what i think this problem is
(setq window-system-default-frame-alist '(
                                          (x (font . "DejaVu Sans Mono-9"))
                                          (ns (font . "DejaVu Sans Mono-12"))))
;; use lots of colors for () in elisp
(apply-mode-hook 'rainbow-delimiters-mode 'emacs-lisp)
;; word wrap
(apply-mode-hook 'visual-line-mode 'text)
;; kill the welcome screen
(setq inhibit-startup-screen t)
;; turn off gui elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; diminish some other modes. i have nowhere else to put these
(diminish 'auto-revert-mode)
(diminish 'undo-tree-mode)

; performance
; big gc threshold recommended by lsp performance guide
(setq gc-cons-threshold (* 100 1024 1024))
; increase the amount that emacs reads from processes
(setq read-process-output-max (* 1024 1024))

; usability
;; stop asking for "yes" and "no"
(defalias 'yes-or-no-p 'y-or-n-p)
;; don't ask at all about following symlinks to version-controlled files
(setq vc-follow-symlinks t)
;; quit confirmation
(setq confirm-kill-emacs 'y-or-n-p)
;; ivy
(ivy-mode 1)
(diminish 'ivy-mode)
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")
(global-set-key (kbd "C-c C-r") 'ivy-resume)
; swiper/searching
(global-set-key "\C-s" 'swiper)
;; enable which-key mode globally
(which-key-mode)
(diminish 'which-key-mode)
;; treat _ as part of a word
(add-to-list 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

; spelling
(diminish 'flyspell-mode)
(apply-mode-hook 'flyspell-mode 'text)
(apply-mode-hook 'flyspell-prog-mode 'prog)
; don't spell check inside strings in prog-mode
(setq flyspell-prog-text-faces
      (delq 'font-lock-string-face flyspell-prog-text-faces))
; enable the ivy minibuffer
(require 'flyspell-correct-ivy)
(define-key flyspell-mode-map (kbd "M-$") 'flyspell-correct-wrapper)

; magit
(global-set-key (kbd "C-c g") 'magit-status)

; specific language settings
;; haskell
(setq haskell-check-command "ghc -fno-code")
(setq haskell-process-args-ghci (quote ("-dynamic" "-ferror-spans")))

;; python
(setq
 ;; lsp-pylsp-server-command (concat user-emacs-directory "lsp-virtualenv/bin/pylsp")
 lsp-pylsp-plugins-pydocstyle-enabled t
 lsp-pylsp-plugins-flake8-enabled t
 lsp-pylsp-plugins-pylint-enabled t)
(add-hook 'python-mode-hook 'lsp)
;(setq elpy-rpc-python-command "python3")
;(require 'flymake-python-pyflakes)
;(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;(setq flymake-python-pyflakes-executable "flake8")

;(require 'evil-leader)
;(global-evil-leader-mode)
;(evil-leader/set-leader ",")
;(evil-leader/set-key
;  "b" 'switch-to-buffer
;  "w" 'save-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(base16-eighties))
 '(custom-safe-themes
   '("9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
