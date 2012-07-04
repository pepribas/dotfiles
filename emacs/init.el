;dd-to-list 'load-path "~/.emacs.d/vendor")
(progn (cd "~/.emacs.d/vendor")
(normal-top-level-add-subdirs-to-load-path));; color themes

(require 'color-theme)
(load "~/.emacs.d/colors/molokai/color-theme-molokai.el")
(color-theme-molokai)
(set-frame-font "-unknown-Monaco for Poweline-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")

;;; powerline
(require 'powerline)

(require 'auto-complete)

(require 'python-mode)

(setq mouse-yank-at-point t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(global-set-key [home] 'beginning-of-line)      ;; emacs-23.3 Apple
(global-set-key [C-home] 'beginning-of-buffer)  ;; emacs-23.3 Apple
(global-set-key [end] 'end-of-line)             ;; emacs-23.3 Apple
(global-set-key [C-end] 'end-of-buffer)         ;; emacs-23.3 Apple

; complete using opened buffers
(global-set-key [M-return] 'dabbrev-expand)

;; turn off annoying foo~ files
(setq make-backup-files nil)

;; turn off annoying #foo# files
(setq auto-save-mode nil)
(setq auto-save-default nil)

;; make all prompts y/n
(fset 'yes-or-no-p 'y-or-n-p)

(require 'pabbrev nil t)
(pabbrev-shut-up)
(pabbrev-global-mode)
;; bind C-z to undo M$ style, rebinding suspend-emacs (or whatever) to C-x C-z
(let ((former-binding (key-binding "\C-z")))
  (global-set-key (kbd "\C-x \C-z") former-binding)
  (global-set-key (kbd "\C-z") 'undo))

;; bind other-window
(global-set-key [C-tab] 'other-window)

(global-set-key [C-S-iso-lefttab] 'other-frame)

(iswitchb-mode 1)
(global-set-key (kbd "\C-b") 'iswitchb-buffer)

(load-file "~/.emacs.d/vendor/simple-overrides/simple-overrides.el")
(setq iswitchb-buffer-ignore '("*Completions*" "*Messages*" "^ " "^irc.freenode.net" "*Minibuf-1*" "TAGS" "*fsm-debug*" "*RNC Input"))
(defun iswitchb-local-keys ()
  (define-key iswitchb-mode-map (kbd "<right>") 'iswitchb-next-match)
  (define-key iswitchb-mode-map (kbd "<left>") 'iswitchb-prev-match))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
(define-key minibuffer-local-map (kbd "<up>") 'my-previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'my-next-complete-history-element)
(autoload 'mcomplete-mode "mcomplete"
  "Toggle minibuffer completion with prefix and substring matching."
  t nil)
(autoload 'turn-on-mcomplete-mode "mcomplete"
  "Turn on minibuffer completion with prefix and substring matching."
  t nil)
(autoload 'turn-off-mcomplete-mode "mcomplete"
  "Turn off minibuffer completion with prefix and substring matching."
  t nil)
(turn-on-mcomplete-mode)

(require 'which-func nil t)
(which-func-mode)
(setq which-func-mode-global t)
;; MOVE AROUND BUFFERS from http://www.onerussian.com/Linux/.files/dot_emacs

(defun yic-ignore (str)
  (or
   ;;buffers I don't want to switch to
   (string-match "\\*Buffer List\\*" str)
   (string-match "^TAGS" str)
   (string-match "^\\*Messages\\*$" str)
   (string-match "^\\*Completions\\*$" str)
   (string-match "^\\*Shell Command Output\\*$" str)
   (string-match "^\\*inferior-lisp\\*$" str)
   (string-match "^irc.freenode.net:6667$" str)
   (string-match "^ " str)))

(defun yic-next (ls)
  "Switch to next buffer in ls skipping unwanted ones."
  (let* ((ptr ls)
	 bf bn go)
    (while (and ptr (null go))
      (setq bf (car ptr)  bn (buffer-name bf))
      (if (null (yic-ignore bn))
          (setq go bf)
        (setq ptr (cdr ptr))))
    (if go
        (switch-to-buffer go))))

(defun yic-next-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (yic-next (buffer-list)))

(global-set-key [C-M-tab] 'yic-next-buffer)

;; END OF MOVE ARUND BUFFERS
