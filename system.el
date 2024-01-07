;;; UTF-8
(prefer-coding-system 'utf-8)

;;; Y-O-N INSTEAD OF YES-OR-NO
(setq use-short-answers t)

;;; BELL: ON/OFF
(setq ring-bell-function 'ignore)

;;; SMOOTH SCROLLING
(pixel-scroll-precision-mode 1)

;;; If you want Emacs to continue to “drift” the display after
;;; it stops, and you aren’t using the NS port on macOS,
;;; enable the following:
;; (pixel-scroll-precision-use-momentum 1)

;;; Apply the following setting if you also want scrolling with
;;; an ordinary mouse to be almost as smooth, as scrolling with
;;; a touchpad on systems other than X
;;; If that number does not work, decrease it by 5 at a time
;;; until it starts to. Keep in mind that setting it too low will
;;; cause normal trackpad scrolling to be interpolated, which is
;;; probably not what you want.
(setq pixel-scroll-precision-large-scroll-height 40.0)

;;; HORIZONTALL SCROLLING WITH MOUSE (REVERSED FOR MAC OS)
;;; to disable the reverse - swap 'scroll-left' and 'scroll-right'
;; (global-set-key [wheel-right] '(lambda ()
;;                                      (interactive)
;;                                      (scroll-left 2)))
;; (global-set-key [wheel-left] '(lambda ()
;;                                      (interactive)
;;                                      (scroll-right 2)))

;;; RIGHT CLICK CONTEXT MENU
(context-menu-mode 1)

;;; UNIQUIFY BUFFER NAMES FOR BUFFERS THAT WOULD HAVE IDENTICAL NAMES
(setq uniquify-buffer-name-style 'forward)

;;; IBUFFER – THE BUFFER MANAGER
(require 'ibuf-ext)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))
(setq ibuffer-marked-face 'dired-marked)

;; Should the *scratch* buffer contain some initial content?
(setq initial-scratch-message "")

;;; MAKE BACKUP BEFORE EDITING
(setq backup-by-copying t
      kept-new-versions 10
      kept-old-versions 3
      delete-old-versions t
      version-control t)
;;; BACKUP DIRECTORY
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
;;; TURN ON RECENT FILE MODE TO VISIT RECENTLY EDITED FILES
(recentf-mode 1)
(setq recentf-max-menu-items 30
      recentf-max-saved-items 30)

;;; TOOL FOR MANAGING OS PROCESS (HTOP IN EMACS)
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
;;; KEYBINDING
(global-set-key (kbd "C-x w w") #'browse-url)

;;; CALENDAR
(require 'calendar)
(setq calendar-date-style 'iso
      calendar-week-start-day 1
      calendar-weekend-days '(6 0))

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
