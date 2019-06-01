;;; package --- init.el
;;; Commentary:
;;; go away flycheck error
;;; Code:

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
Example: (apply-mode-hook 'flycheck-mode \"emacs-lisp\" \"haskell\")"
  (mapc (lambda (it)
          (add-hook it hook))
        (apply 'make-mode-hooks modes)))

; ----- Package bootstrap -----
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package base16-theme)
(use-package company)
(use-package company-go)
(use-package company-jedi)
(use-package company-shell)
(use-package evil)
(use-package evil-leader)
(use-package evil-numbers)
(use-package evil-quickscope)
(use-package evil-surround)
(use-package flycheck-gometalinter)
(use-package flycheck-haskell)
(use-package flycheck-yamllint)
(use-package flymake)
(use-package flymake-css)
(use-package flymake-cursor)
(use-package flymake-haskell-multi)
(use-package flymake-json)
(use-package flymake-python-pyflakes)
(use-package flymake-ruby)
(use-package flymake-shell)
(use-package ghc)
(use-package ghc-imported-from)
(use-package go-mode)
(use-package haskell-mode)
(use-package jedi)
(use-package jedi-core)
(use-package jinja2-mode)
(use-package markdown-mode)
(use-package magit)
(use-package ox-hugo :after ox)
(use-package pandoc)
(use-package pandoc-mode)
(use-package rainbow-delimiters)
(use-package salt-mode)
(use-package which-key)
;; must come after evil and magit
(use-package evil-magit)

(byte-recompile-directory "~/.emacs.d/lisp" 0)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'flycheck-emacs-lisp-load-path "~/.emacs.d/lisp")
(require 'junos-mode)

(evil-mode t)
(use-package evil-surround :config (global-evil-surround-mode 1))
(setq evil-highlight-closing-paren-at-point-states '(not emacs insert replace normal visual))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

; completion
(apply-mode-hook 'company-mode 'prog)

; syntax/style checking
(apply-mode-hook 'flycheck-mode 'prog)
(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

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

; usability
;; stop asking for "yes" and "no"
(defalias 'yes-or-no-p 'y-or-n-p)
;; don't ask at all about following symlinks to version-controlled files
(setq vc-follow-symlinks t)
;; quit confirmation
(setq confirm-kill-emacs 'y-or-n-p)
;; ido-mode
(ido-mode)
(ido-everywhere t)
;; enable which-key mode globally
(which-key-mode)

; spelling
(apply-mode-hook 'flyspell-mode 'text)
(apply-mode-hook 'flyspell-prog-mode 'prog)
; don't spell check inside strings in prog-mode
(setq flyspell-prog-text-faces
      (delq 'font-lock-string-face flyspell-prog-text-faces))

; magit
(global-set-key (kbd "C-c g") 'magit-status)

; specific language settings
;; haskell
(setq flycheck-ghc-args '("-dynamic"))
(setq flycheck-haskell-runghc-command '("runghc -dynamic"))
(setq haskell-check-command "ghc -fno-code")
(setq haskell-process-args-ghci (quote ("-dynamic" "-ferror-spans")))

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
 '(custom-enabled-themes (quote (base16-eighties)))
 '(custom-safe-themes
   (quote
    ("9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
