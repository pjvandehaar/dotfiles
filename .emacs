(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; info:python-mode 
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

;; http://stackoverflow.com/questions/5710334/how-can-i-get-mouse-selection-to-work-in-emacs-and-iterm2-on-mac?rq=1
'(progn
   (require 'mouse)
   (xterm-mouse-mode t)
   (defun track-mouse (e))
   (setq mouse-sel-mode t))

