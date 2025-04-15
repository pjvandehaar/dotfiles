(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((comment-start . %)))
 '(safe-local-variable-values '((select-enable-clipboard nil)))  ;; emacs29 made killring = mac clipboard.  this separates them again.
 '(safe-local-variable-values '((x-select-enable-clipboard nil)))  ;; emacs29 made killring = mac clipboard.  this separates them again.
)

(setq select-enable-clipboard nil)
(setq x-select-enable-clipboard nil)


;;; Emacs third-party packages
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

(when (require 'whitespace)  ;; see `C-h v whitespace-style`
  (setq-default whitespace-style '(face tabs trailing indentation::space space-before-tab space-after-tab))
  (global-whitespace-mode))



;;;; Emacs filetype-specific ;;;;
;;(require 'git)  ;; I don't think I use these anymore, might as well speed up startup
;;(require 'git-blame)

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
(add-hook 'python-mode-hook 'infer-indentation-style)  ;; in python, check whether to use <tab> or "    "

;;(setq-default scroll-margin 1)  ;; Move screen when cursor is within 2 lines of top/bottom.  I wish I could set top=1 bottom=6.
;;(setq-default scroll-step 2)  ;; When scroll is triggered, move two lines.  If cursor is still out-of-bounds, center cursor.  "1" centers when scrolling fast.

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))  ;; Disable electric-indent-mode due to pasting problems.  If you want to open a new line that's auto-indented, use C-j.

(delete-selection-mode) ;; If you have a selection, typing deletes it.  As does C-d.  Doesn't copy into kill-ring.




;;;; Emacs custom commands ;;;;
(global-set-key (kbd "C-x t") 'insert-tab)
(defun insert-tab ()
  (interactive)
  (insert-char 9))

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

;; Select inside matching curly braces
(defun select-region-inside-matching-delims ()
  "Select text inside the smallest surrounding delims."
  ;;; TODO: If we start on a quote, and we don't already have a region, then match to the nearest quote (not just the next)
  (interactive)
  (let ((start-point (point))
        (use-region (and (region-active-p) (> (point) (region-beginning)))))
    (condition-case nil
        (progn
          ;; If we have an active region, move to one character before the start
          (when use-region
            (goto-char (1- (region-beginning)))
            (deactivate-mark))
          
          ;; Find the start of region by moving up the syntax tree.
          ;; Emacs says that this depends on the language.  I hope it works ok in all my files!
          (unless (memq (char-after) '(?\( ?\{ ?\< ?\[ ?\" ?\'))
            (backward-up-list))
          (push-mark (point))

          ;; Find end of region.
          (if (memq (char-after) '(?\" ?'))
            ;; If we're at a quote (single or double), look forward to the matching quote.
            (let ((delim (char-after)))
              (forward-char 1)
              (search-forward (string delim))
              (backward-char 1))
            ;; Otherwise find the closing brace
            (progn
              (forward-list)
              (backward-char 1)))
          (activate-mark))
      (error 
       (message "Failed to select a region.")
       (goto-char start-point)))))
(global-set-key (kbd "M-i") 'select-region-inside-matching-delims)


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
