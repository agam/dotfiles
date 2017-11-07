;;;; Meta-note: useful packages to install
; helm
; helm-swoop
; clang-format
; yasnippet
; yasnippet-snippets
; helm-ag
; powerline
; magit
; smartparens
; sly
; sly-company
; sly-quicklisp
; sly-named-readtables
; sly-macrostep

;; Common lisp stuff
(require 'cl)

;; Replace meta sequence
;; Update: replaced with Helm!
;; (global-set-key "\C-x\C-m" 'execute-extended-command)

;; Backspace improvement
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-c\C-k" 'kill-region)

;; Fonts and colors
(global-font-lock-mode t)

;; "Lose the UI"
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Installing packages is better than downloading and copy-pasting .el files
(require 'package)
(package-initialize)

;; Add melpa and marmalade to the mix
(setq package-archives '(("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))

;; Set solarized theme
;; Note: To get this, M-x package-install -> solarized-theme
(load-theme 'solarized t)

;; Lisp
;; (require 'slime)

;; We don't use SLIME, coz we SLY!
(setq inferior-lisp-program "/usr/bin/sbcl")

(require 'sly-autoloads)
(require 'sly-quicklisp-autoloads)

(add-hook 'sly-mode-hook 'sly-company-mode)
(add-to-list 'company-backends 'sly-company)
(require 'sly-company)

;; When would I _not_ want column numbers ??
(column-number-mode t)

;; Ditto for matching up parentheses
(show-paren-mode t)

;; Please don't show me the ugly "^L" characters
(require 'pp-c-l)
(pretty-control-l-mode t)

;; Don't create backup files -- I have _version control_ for that
(setq make-backup-files nil)

;; Make picking buffers and files a better experience
(require 'helm-config)
(helm-mode 1)

;; Use my M-x key sequence to trigger Helm instead of the bultin
(global-set-key (kbd "C-x C-m") 'helm-M-x)

;; Saner undo/redo model
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Allow multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; When I select a region and start typing, I obviously want
;; to replace the text I've just selected (and I want to
;; delete it, not add it to the kill ring (if I wanted to
;; kill it, I would do this explicitly)
(delete-selection-mode t)

;; The default fill-column of 70 is pointless; whenever I care
;; about the "wrap width" of lines in my code, I usually want 80.
(set-fill-column 80)

;; Ctrl-z makes the frame freeze ... why would I _ever_ want this?
(put 'suspend-frame 'disabled t)

;; Quickly fix long lines
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key "\C-cf" 'fill-paragraph)))

;;;;;;;;;;;;;;;;;;;;
;; Refactoring support
(require 'srefactor)
(semantic-mode 1)

;;;;;;;;;;;;;;;;;;;;
;; Snippets, because I am _sick_ of repeatedly typing in some stuff.
(require 'yasnippet)
(yas-global-mode 1)
;; MEMO: Install the `yasnippets-snippets` package to get some good defaults.

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

;; Window navigation (moved to the end to overwrite conflicting assignments)
(global-set-key "\C-x\C-h" 'windmove-left)
(global-set-key "\C-x\C-j" 'windmove-down)
(global-set-key "\C-x\C-k" 'windmove-up)
(global-set-key "\C-x\C-l" 'windmove-right)
