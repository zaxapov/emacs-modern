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


(load "~/emacs-modern/system.el")
(load "~/emacs-modern/keybindings.el")
(load "~/emacs-modern/modeline_minibuffer.el")
(load "~/emacs-modern/text-editing.el")
(load "~/emacs-modern/languages.el")
(load "~/emacs-modern/visual-spectacle.el")

(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
;; NOERROR to ignore nonexistent file - Emacs will create it
(load custom-file t)
