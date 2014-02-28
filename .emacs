;; Common lisp stuff
(require 'cl)

;; Replace meta sequence
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; Backspace improvement
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-x\C-k" 'kill-region)

;; Fonts and colors
(global-font-lock-mode t)

;; "Lose the UI"
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Lisp
(require 'slime)
(slime-setup)