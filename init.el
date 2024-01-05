;; Installing straight.el package manager
;; https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure use-package to use with straight.el
(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))

(load "~/emacs-modern/keybindings.el")
(load "~/emacs-modern/visual-spectacle.el")
(load "~/emacs-modern/system.el")
(load "~/emacs-modern/modeline_minibuffer.el")
(load "~/emacs-modern/text-editing.el")
(load "~/emacs-modern/languages.el")
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(blink-cursor-mode nil)
;;  '(column-number-mode t)
;;  '(custom-safe-themes
;;    '("d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563" default))
;;  '(size-indication-mode t)
;;  '(tool-bar-mode nil))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

