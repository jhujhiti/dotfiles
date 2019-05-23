;;; package --- init.el
;;; Commentary:
;;; go away flycheck error
;;; Code:

; ----- Helper functions -----
(defun make-mode-hooks (&rest modes)
  "Concatenates -mode-hook onto the end of each argument (MODES)
and returns them as interned symbols."
  (mapcar (lambda (it)
            (intern (concat it "-mode-hook")))
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
(use-package markdown-mode)
(use-package ox-hugo :after ox)
(use-package pandoc)
(use-package pandoc-mode)
(use-package rainbow-delimiters)
(use-package salt-mode)


(evil-mode t)
(use-package evil-surround :config (global-evil-surround-mode 1))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

; completion
(apply-mode-hook 'company-mode "prog")

; syntax/style checking
(apply-mode-hook 'flycheck-mode "prog")
(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

; beautification
;; basic look-and-feel
; Mac OS (ns) DPI scaling is a pain. or at least that's what i think this problem is
(setq window-system-default-frame-alist '(
                                          (x (font . "DejaVu Sans Mono-9"))
                                          (ns (font . "DejaVu Sans Mono-12"))))
;; stop asking for "yes" and "no"
(defalias 'yes-or-no-p 'y-or-n-p)
;; don't ask at all about following symlinks to version-controlled files
(setq vc-follow-symlinks t)
;; use lots of colors for () in elisp
(apply-mode-hook 'rainbow-delimiters-mode "emacs-lisp")
;; word wrap
(apply-mode-hook 'visual-line-mode "text")

; spelling
(apply-mode-hook 'flyspell-mode "text")
(apply-mode-hook 'flyspell-prog-mode "prog")
; don't spell check inside strings in prog-mode
(setq flyspell-prog-text-faces
      (delq 'font-lock-string-face flyspell-prog-text-faces))

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
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-enabled-themes (quote (base16-eighties)))
 '(custom-safe-themes
   (quote
    ("9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" default)))
 '(evil-highlight-closing-paren-at-point-states (quote (not emacs insert replace normal visual)))
 '(flycheck-ghc-args (quote ("-dynamic")))
 '(flycheck-haskell-runghc-command (quote ("runghc -dynamic")))
 '(haskell-check-command "ghc -fno-code")
 '(haskell-compile-command "ghc -Wall -ferror-spans -dynamic -fforce-recomp -c %s")
 '(haskell-process-args-ghci (quote ("-dynamic" "-ferror-spans")))
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
