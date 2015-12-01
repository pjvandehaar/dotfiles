(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/dotfiles/third-party/emacs")
(require 'git)
(require 'git-blame)

;; I think that `setq-default` will let modes that override this setting do so.
(setq-default vc-follow-symlinks t)
(setq-default enable-local-variables nil)
(setq-default indent-tabs-mode nil)
(setq-default mode-require-final-newline nil)

(column-number-mode)
(show-paren-mode)

(which-func-mode)
(eval-after-load "which-func"
  '(setq which-func-modes '(java-mode c++-mode perl-mode)))

;; see `C-h v whitespace-style`
(require 'whitespace)
(setq-default whitespace-style '(face trailing indentation::space space-before-tab space-after-tab))
(global-whitespace-mode)

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
