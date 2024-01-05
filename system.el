;;; SYSTEM
;; UTF-8
(prefer-coding-system 'utf-8)

;; y-o-n instead of yes-or-no
(setq use-short-answers t)

;; Bell: on/off
(setq ring-bell-function 'ignore)

;;; SMOOTH SCROLLING
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1) ;; Pixel-based scrolling
  ;; Enable next option only on Linux, Windows and some Emacs ports for macOS
  ;; (pixel-scroll-precision-use-momentum 1) ;; Emacs to continue to “drift” the display after it stops
  (setq pixel-scroll-precision-large-scroll-height 40.0)) ;; emulate smooth scrolling with ordinary mouse

;;; HORIZONTALL SCROLLING WITH MOUSE (REVERSED FOR MAC OS)
;; (global-set-key [wheel-right] '(lambda ()
;;                                      (interactive)
;;                                      (scroll-left 2)))
;; (global-set-key [wheel-left] '(lambda ()
;;                                      (interactive)
;;                                      (scroll-right 2)))

;; Right click context menu
(context-menu-mode 1)

;; Uniquify buffer names for buffers that would have identical names
(setq uniquify-buffer-name-style 'forward)

;;; IBUFFER – the buffer manager
(require 'ibuf-ext)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))
(setq ibuffer-marked-face 'dired-marked)
;; Don't show the boring buffers in Ibuffer

;; Should the *scratch* buffer contain some initial content?
(setq initial-scratch-message "")

;;; BACKUP
;; Make backup before editing
(setq backup-by-copying t
      kept-new-versions 10
      kept-old-versions 3
      delete-old-versions t
      version-control t)

;; Where to save the backups?
;; Specify file name/path patterns and directories ("REGEXP" . "DIRECTORY")
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))

;;; LOCKFILES
;; Let Emacs keep track of files currently visited?
(setq create-lockfiles nil)

;;; AUTO-SAVE
(setq auto-save-default nil
      auto-save-interval 0)

;;; RECENT FILES
(require 'recentf)
;; Turn on recent file mode to visit recently edited files
(recentf-mode 1)
(setq recentf-max-menu-items 30
      recentf-max-saved-items 30)

;;; PROCED
;; Manage OS processes
(require 'proced)
(setq proced-auto-update-interval 1)
(setq-default proced-auto-update-flag t
              proced-descend t)

;;; NET-UTILS
(require 'net-utils)
(setq netstat-program "netstat"
      netstat-program-options '("-atupe"))

;;; USE DEFAULT WEB BROWSER
(setq browse-url-browser-function #'browse-url-default-browser)
;; Keybinding
(global-set-key (kbd "C-x w w") #'browse-url)

;;; CALENDAR
(require 'calendar)
(setq calendar-date-style 'iso
      calendar-week-start-day 1
      calendar-weekend-days '(6 0))
