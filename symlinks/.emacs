;; TODO: I want something like vim's `ciw` (change inside word, change inside quotes, change inside parens, etc).
;;       Bind this to M-i
;;       Things I want to select/delete inside are:
;;          + directional wrappers: ({[< >]})
;;          + directionless wrappers: '"` \s \b
;;       Option 1: `M-i delim` selects to prev and next <delim>.
;;       Option 2: `M-i` selects the smallest valid area (whichever of the 9 delimiters gives that). Maybe drop \s and \b
;;                 If we currently have a selection, the selection must grow on both ends.  Else, it's okay to use that pointer as an endpoint.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(rust-mode expand-region))
 '(safe-local-variable-values '((comment-start . %))))

;; Put backups in ~/.emacs-saves/ instead of literring all over.
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)

(setq split-width-threshold 200) ;; Split side-by-side if >200 cols, else above-and-below.  Default=160.

;; Packaging:
;; when first installing this .emacs, do `M-x package-install <return> <package-name>`
;; sometimes do: `M-x list-packages` to check for updates
;; When you install a new package, you must `(require '<package-name>)` here.
(when (>= emacs-major-version 24)
  (require 'package)
  ;; MELPA-stable is the best repo. It installs the latest tagged commit.
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize)
  (if (require 'expand-region nil 'noerror)
    (global-set-key (kbd "C-]") 'er/expand-region)
    (message "failed to load expand-region"))
  ;; I'm not interested in this right now.  Maybe enable it later.
  ;; ;; when you move one-line-at-a-time (eg, C-n repeatedly) (or even a few lines at a time),  this will scroll the screen 5 lines ahead of you.
  ;; (if (require 'smooth-scrolling nil 'noerror)
  ;;   (setq smooth-scroll-margin 5)
  ;;   (smooth-scrolling-mode))
)

(menu-bar-mode -1) ;; Disable menubar

(add-to-list 'load-path "~/dotfiles/third-party/emacs")
(require 'git)
(require 'git-blame)
(autoload 'nginx-mode "nginx-mode" nil t)
(setq nginx-indent-level 2)

;; Disable electric-indent-mode (due to pasting problems mostly).  If you want auto-indent, use C-j.
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

;; Disable change-log-mode for `CHANGELOG.md`
(rassq-delete-all 'change-log-mode auto-mode-alist)

;; I think that `setq-default` will let modes that override this setting do so.
(setq-default vc-follow-symlinks t)
;; (setq-default enable-local-variables nil) ;; this line prevents handling lines that declare file type
(setq-default indent-tabs-mode nil) ;; never insert a \t.  Always spaces.
(setq-default mode-require-final-newline nil) ;; don't auto-add \t at EOF, b/c it gets annoying.

(column-number-mode) ;; show column in mode-line
(show-paren-mode) ;; match parens

(delete-selection-mode) ;; C-d deletes current selection without copying into kill-ring

;; see `C-h v whitespace-style`
(require 'whitespace)
(setq-default whitespace-style '(face trailing indentation::space space-before-tab space-after-tab))
(global-whitespace-mode)

;; mouse-scrolling to move screen & pointer. copied from <https://iterm2.com/faq.html> & modified.
;; TODO: I can't figure out how to retain my improved scrolling while eliminating emacs control of click-and-drag selection
;; (require 'mwheel)
;; (require 'mouse)
;; (xterm-mouse-mode t) ;; this enables selection-by-mouse, which is sometimes convenient but usually bothersome
;; (mouse-wheel-mode t)
;; (global-set-key [mouse-4] 'previous-line) ;; These move the cursor
;; (global-set-key [mouse-5] 'next-line)
;; (global-set-key [mouse-4] 'scroll-down-line) ;; These move the screen & drag the cursor.
;; (global-set-key [mouse-5] 'scroll-up-line)
;; (global-set-key [mouse-4] (lambda () (interactive) (scroll-down-line) (set-window-point (selected-window) (window-start)))) ;; These move the screen & put the cursor at the beginning/end
;; (global-set-key [mouse-5] (lambda () (interactive) (scroll-up-line) (set-window-point (selected-window) (window-end))))

;; go-to-char
(global-set-key (kbd "C-t") 'go-to-char)
(global-set-key (kbd "M-t") 'go-back-to-char)
(setq gtc--dir 1 gtc--char ?\ )
(defun go-to-char (char)
  (interactive "cGo to char: ")
  (if (= char 20)
      (gtc--internal gtc--char)
    (setq gtc--dir 1 gtc--char char)
    (gtc--internal char)))
(defun go-back-to-char (char)
  (interactive "cGo back to char: ")
  (setq gtc--dir -1 gtc--char char)
  (gtc--internal char))
(defun gtc--internal (char)
  (condition-case err
      (progn
        (forward-char gtc--dir)
        (caseful-search gtc--char gtc--dir)
        (when (= 1 gtc--dir) (forward-char -1)))
    (error (signal (car err) (cdr err)))))
(defun caseful-search (char dir)
  (let ((old-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    (search-forward (string char) nil nil dir)
    (setq case-fold-search old-case-fold-search)))

;; toggle between horizontal and vertical split.  Useful after `emacs foo bar`.
(defun toggle-window-split ()
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let* ((next-win-buffer (window-buffer (next-window)))
         (previously-vertical (window-combined-p)))
    (delete-other-windows)
    (if previously-vertical (split-window-horizontally) (split-window-vertically))
    (set-window-buffer (next-window) next-win-buffer)))
