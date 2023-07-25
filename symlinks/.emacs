(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((comment-start . %))))


;;;; Emacs third-party packages
(add-to-list 'load-path "~/dotfiles/third-party/emacs")
;; Right now I'm not using any packages that aren't built-in.
;; When first installing this as `~/.emacs`, do `M-x package-install <return> <package-name>` for each package you want.
;; Ocasionally run `M-x list-packages` to check for updates
;; When you install a new package, you must `(require '<package-name>)` here.
;; (when (>= emacs-major-version 24)
;;   (require 'package)
;;   (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)  ;; MELPA-stable is the best repo. It installs the latest tagged commit.
;;   (package-initialize)
;; )



;;;; Emacs file-system interaction ;;;;
(setq backup-directory-alist `(("." . "~/.emacs-saves")))  ;; Put backups in ~/.emacs-saves/ instead of literring all over.
(setq backup-by-copying t)
(setq-default vc-follow-symlinks t)



;;;; Emacs visuals ;;;;
(setq split-width-threshold 200) ;; Split side-by-side if >200 cols, else above-and-below.  Default=160.
(menu-bar-mode -1) ;; Hide menubar
(column-number-mode) ;; show column in mode-line
(show-paren-mode) ;; highlights matching paren

(if (require 'whitespace)  ;; see `C-h v whitespace-style`
  (setq-default whitespace-style '(face trailing indentation::space space-before-tab space-after-tab))
  (global-whitespace-mode))



;;;; Emacs filetype-specific ;;;;
(require 'git)
(require 'git-blame)

(autoload 'nginx-mode "nginx-mode" nil t)  ;; load ~/dotfiles/third-party/emacs/nginx-mode.el
(setq nginx-indent-level 2)

(rassq-delete-all 'change-log-mode auto-mode-alist)  ;; Disable change-log-mode for `CHANGELOG.md`



;;;; Emacs interaction ;;;;

;; (if (require 'expand-region nil 'noerror)  ;; expand-region is decent, but often spends several iterations trapped on a few letters
;;   (global-set-key (kbd "C-]") 'er/expand-region)
;;   (message "failed to load expand-region"))

(setq-default tab-width 4) ;; <tab> inserts four spaces, and tabs are displayed four-columns-wide
(setq-default mode-require-final-newline nil) ;; don't auto-add \n at EOF, b/c it gets annoying.
(setq-default indent-tabs-mode nil) ;; <tab> inserts spaces, not \t
(defun infer-indentation-style ()
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))
(add-hook 'python-mode-hook 'infer-indentation-style)

(setq-default scroll-margin 4)  ;; Move screen when cursor is within 4 lines of top/bottom.  I wish I could set top=1 bottom=6.
(setq-default scroll-step 2)  ;; When scroll is triggered, move two lines.  If cursor is still out-of-bounds, center cursor.  "1" centers when scrolling fast.

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))  ;; Disable electric-indent-mode due to pasting problems.  If you want to open a new line that's auto-indented, use C-j.

(delete-selection-mode) ;; If you have a selection, typing deletes it.  As does C-d.  Doesn't copy into kill-ring.




;;;; Emacs custom commands ;;;;
(global-set-key (kbd "C-t") 'go-to-char)
(global-set-key (kbd "M-t") 'go-back-to-char)
(setq gtc--direction 1 gtc--char ?\ )
(defun go-to-char (char)
  (interactive "cGo to char: ")
  (if (= char 20)  ;; "C-t" is 20
      (gtc--internal)
    (setq gtc--direction 1 gtc--char char)
    (gtc--internal)))
(defun go-back-to-char (char)
  (interactive "cGo back to char: ")
  (setq gtc--direction -1 gtc--char char)
  (gtc--internal))
(defun gtc--internal ()
  (condition-case err
      (progn
        (forward-char gtc--direction)
        (caseful-search gtc--char gtc--direction)
        (when (= 1 gtc--direction) (forward-char -1)))
    (error (signal (car err) (cdr err)))))
(defun caseful-search (char direction)
  (let ((case-fold-search nil))  ;; temporarily set case-fold-search=nil
    (search-forward (string char) nil nil direction)))

;; toggle between horizontal and vertical split.  Useful after `emacs foo bar`.
(defun toggle-window-split ()
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let* ((next-win-buffer (window-buffer (next-window)))
         (previously-vertical (window-combined-p)))
    (delete-other-windows)
    (if previously-vertical (split-window-horizontally) (split-window-vertically))
    (set-window-buffer (next-window) next-win-buffer)))

;; TODO: I want vim's `ci(` (change-inside-parens, change-inside-quotes, etc).
;;       Option 1: `M-i delim` selects to prev and next delim, for delim in ({[< '"` space
;;       Option 2: `M-i` selects the smallest valid area, whichever of ({[<'"` .  This is similar to expand-region but without \b\s delims.

;; mouse-scrolling to move screen & pointer. copied from <https://iterm2.com/faq.html> & modified.
;; I can't figure out how to retain my improved scrolling but also keeping click-and-drag selection at terminal level, not emacs.
;; (require 'mwheel)
;; (require 'mouse)
;; (mouse-wheel-mode t)
;; (global-set-key [mouse-4] 'previous-line) ;; These move the cursor
;; (global-set-key [mouse-5] 'next-line)
;; (global-set-key [mouse-4] 'scroll-down-line) ;; These move the screen & drag the cursor.
;; (global-set-key [mouse-5] 'scroll-up-line)
;; (global-set-key [mouse-4] (lambda () (interactive) (scroll-down-line) (set-window-point (selected-window) (window-start)))) ;; These move the screen & put the cursor at the beginning/end
;; (global-set-key [mouse-5] (lambda () (interactive) (scroll-up-line) (set-window-point (selected-window) (window-end))))
