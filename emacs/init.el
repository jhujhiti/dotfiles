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
(use-package diminish)
(use-package evil)
(use-package evil-leader)
(use-package evil-magit :after (evil magit))
(use-package evil-numbers)
(use-package evil-quickscope)
(use-package evil-surround)
(use-package flymake)
(use-package flymake-css)
;; Causes errors, not interested in why right now
;; (use-package flymake-cursor)
(use-package flymake-haskell-multi)
(use-package flymake-json)
(use-package flymake-python-pyflakes)
(use-package flymake-ruby)
(use-package flymake-shell)
(use-package flyspell-correct-ivy)
(use-package forge :after magit)
(use-package gitignore-mode)
(use-package ghc)
(use-package ghc-imported-from)
(use-package go-mode)
(use-package haskell-mode)
(use-package inf-ruby)
(use-package ivy)
(use-package jedi)
(use-package jedi-core)
(use-package jinja2-mode)
(use-package markdown-mode)
(use-package magit)
(use-package ox-hugo :after ox)
(use-package pandoc)
(use-package pandoc-mode)
(use-package rainbow-delimiters)
(use-package rubocop)
(use-package salt-mode)
(use-package smart-tabs-mode)
(use-package which-key)

(byte-recompile-directory "~/.emacs.d/lisp" 0)
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'junos-mode)

(if (file-directory-p "~/.emacs.d/site-lisp")
    (progn
      (byte-recompile-directory "~/.emacs.d/site-lisp" 0)
      (add-to-list 'load-path "~/.emacs.d/site-lisp")
      (require 'site-lisp)))

(evil-mode t)
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
;; insert pairs (eg., "()") automatically
(apply-mode-hook 'electric-pair-mode 'prog)

; syntax/style
(apply-mode-hook 'flymake-mode 'prog)
;; tabs and indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default c-default-style "k&r")
(smart-tabs-insinuate 'c 'c++ 'python 'cperl 'nxml)
;; we want to enable indenting with tabs on the modes above
(apply-mode-hook (lambda ()
                   (setq indent-tabs-mode t)
                   (setq tab-width (default-value 'tab-width)))
                 'c 'c++ 'python 'cperl 'nxml)
(smart-tabs-advice python-indent-line-1 python-indent)

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
;; enable which-key mode globally
(which-key-mode)
(diminish 'which-key-mode)

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
    ("9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" default)))
 '(package-selected-packages
   (quote
    (rubocop inf-ruby gitignore-mode which-key use-package smart-tabs-mode salt-mode rainbow-delimiters poly-ansible pandoc-mode pandoc ox-hugo jedi go-mode ghc-imported-from ghc forge flyspell-correct-ivy flymake-shell flymake-ruby flymake-python-pyflakes flymake-json flymake-haskell-multi flymake-css evil-surround evil-quickscope evil-numbers evil-magit evil-leader diminish company-shell company-jedi company-go base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
