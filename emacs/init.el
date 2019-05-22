;;; package --- init.el
;;; Commentary:
;;; go away flycheck error
;;; Code:
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

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(use-package which-key :ensure t)

(require 'evil)
(evil-mode t)
(use-package evil-surround :ensure t :config (global-evil-surround-mode 1))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

; completion
(apply-mode-hook 'company-mode "prog")

; syntax/style checking
(apply-mode-hook 'flycheck-mode "prog")
(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

; beautification
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

(use-package ox-hugo :ensure t :after ox)

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
 '(package-selected-packages
   (quote
    (which-key salt-mode company-jedi jedi jedi-core flycheck-gometalinter company-go go-mode ox-hugo rainbow-delimiters flycheck-haskell use-package evil-quickscope evil-surround evil-numbers company company-shell flymake flymake-css flymake-cursor flymake-json flymake-python-pyflakes flymake-ruby flymake-shell markdown-mode pandoc pandoc-mode flycheck-ghcmod flymake-haskell-multi ghc ghc-imported-from ghci-completion haskell-mode 2048-game base16-theme evil-leader evil)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "PfEd" :family "DejaVu Sans Mono")))))
;;; init.el ends here
