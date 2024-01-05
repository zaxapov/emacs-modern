;; Themes
;; -------
(use-package soothe-theme)

(use-package ef-themes
  :config
  (load-theme 'ef-winter t))

(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t)
  ;; :config
  ;; (load-theme 'lambda-dark t)
  )

;; Horizontal scroll bar
(horizontal-scroll-bar-mode t)

;;; INITIAL FRAME
;; … or set the default width of the Emacs frame in characters
(add-to-list 'default-frame-alist '(width . 90))
;; … and set the default height of the Emacs frame in lines
(add-to-list 'default-frame-alist '(height . 38))

;;; CURSOR
(add-to-list 'default-frame-alist '(cursor-type . bar))

;; Turn on/off cursor blinking by default?
(blink-cursor-mode -1)  ; 1 means 'on' / -1 means 'off'

;; Cursor blinking interval in seconds
(setq blink-cursor-interval 0.4)

;; Emphasize the cursor when running Emacs in a text terminal?
(setq visible-cursor nil)

;; Make sure to highlight the current line only in the active window.
(setq hl-line-sticky-flag nil)

;;; USER INTERFACE

(menu-bar-mode 1)

;; Scroll bar: on/off by default?
(if (fboundp 'scroll-bar-mode)  ; Emacs 26.1 compatibility
    (scroll-bar-mode -1))

;; Tool bar: on/off by default?
(if (fboundp 'tool-bar-mode)  ; Emacs 26.1 compatibility
    (tool-bar-mode -1))

;; Tooltips: enable/disable?
(tooltip-mode -1)

;; Startup screen: on/off by default?
(setq inhibit-startup-screen t)

;; Alarms: turn off?
(setq ring-bell-function 'ignore)

;; Redraw the display – useful when running Emacs in a Windows terminal emulator
(global-set-key (kbd "C-x r d") #'redraw-display)


;;; FONTS
;;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Fonts>

;; Set default font
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t)))
 ((eq system-type 'darwin) ;; macOS
  (when (member "Luculent Style" (font-family-list))
    ;; (set-face-attribute 'default nil :font "Luculent Style" :height 160))
    (set-frame-font "Luculent Style-16" t t)
    (set-face-attribute 'default nil :width 'semi-condensed)
    )
  )
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono" t t))))