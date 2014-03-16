;; Common lisp stuff
(require 'cl)

;; Replace meta sequence
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; Backspace improvement
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-c\C-k" 'kill-region)

;; Window navigation
(global-set-key "\C-x\C-h" 'windmove-left)
(global-set-key "\C-x\C-j" 'windmove-down)
(global-set-key "\C-x\C-k" 'windmove-up)
(global-set-key "\C-x\C-l" 'windmove-right)

;; Fonts and colors
(global-font-lock-mode t)

;; "Lose the UI"
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Lisp
(require 'slime)
(setq slime-lisp-implementations
      '((ccl ("/usr/local/google/home/agam/bin/ccl64"))))
(slime-setup)

;; When would I _not_ want column numbers ??
(column-number-mode t)

;; Ditto for matching up parentheses
(show-paren-mode t)

;; Please don't show me the ugly "^L" characters
(pretty-control-l-mode t)

;; Make picking buffers and files a better experience
(require 'ido)
(ido-mode t)

;; Quickly fix long lines
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key "\C-cf" 'fill-paragraph)))

;;;;;;;;;;;;;;;;;;;;
;; Defined functions
;;;;;;;;;;;;;;;;;;;;

;; "smart tab", or the poor man's autocomplete
;; Coped from http://www.emacswiki.org/emacs/TabCompletion
(defun smart-tab ()
  "If at the end of minibuffer, or at end of line,
act like usual 'tab'.
If a region is selected, indent it.
Otherwise, expand current symbol"
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil))
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (dabbrev-expand nil)
        (indent-for-tab-command)))))

;; For now, use this only for c++
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "TAB") 'smart-tab)))

(defun end-of-line-and-newline ()
  "Create a new line below and jump to it"
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key "\C-o" 'end-of-line-and-newline)

(defun back-to-previous-line ()
  "Delete back to beginning of current line, and then one
character more, leaving POINT at the end of the previous line."
  (interactive)
  (let ((cur-point (point)))
    (forward-line 0)
    (forward-char -1)
    (delete-region (point) cur-point)))
(global-set-key "\C-cu" 'back-to-previous-line)