;;; DEFAULT FRAME SIZE
(add-to-list 'default-frame-alist '(width . 90))
(add-to-list 'default-frame-alist '(height . 50))
(set-frame-position (selected-frame) 30 50)

;; Startup screen: on/off
(setq inhibit-startup-screen t)

;; SET DEFAULT FONT
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t)))
 ((eq system-type 'darwin) ;; macOS
  (when (member "Luculent Style" (font-family-list))
    ;; (set-face-attribute 'default nil :font "Luculent Style" :height 160))
    (set-frame-font "Luculent Style-16" t t)
    (set-face-attribute 'default nil :width 'semi-condensed)))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono" t t))))

;; THEMES
(use-package ef-themes)
(use-package tron-legacy-theme
  :config
  (setq tron-legacy-theme-vivid-cursor t))

;;; AUTO-SET THEMES BASED ON SYSTEM STYLE
(setq auto-dark-dark-theme 'tron-legacy)
(setq auto-dark-light-theme 'ef-frost)
(use-package auto-dark
  :config (auto-dark-mode t))

;; Cursor type
(add-to-list 'default-frame-alist '(cursor-type . bar))
;; Cursor Blinking: on/off
(blink-cursor-mode -1)
;; Cursor blinking interval in seconds
(setq blink-cursor-interval 0.4)

;; Highlight current line
(hl-line-mode -1)
;; Make sure to highlight the current line only in the active window.
(setq hl-line-sticky-flag nil)

;; Horizontal scroll bar
(horizontal-scroll-bar-mode t)
;; Scroll bar: on/off
(scroll-bar-mode -1)
;; Menu bar: on/off
(menu-bar-mode 1)
;; Tool bar: on/off
(tool-bar-mode -1)
;; Tooltips: on/off
(tooltip-mode -1)

;;; How to display matching parens
(setq show-paren-style 'expression
      show-paren-delay 0.0)

;;; LINE NUMBERS FOR PROG/TEXT-MODE ONLY
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)))
(add-hook 'text-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)))

;;; TURN ON/OFF LINE WRAPPING
;;; OFF
;; (set-default 'truncate-lines 1)
;;; ON
(global-visual-line-mode t)

;;; INDICATE TRAILING WHITESPACE IN PROGRAMMING/TEXT MODES
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace 1)))
(add-hook 'text-mode-hook
          (lambda ()
            (setq show-trailing-whitespace 1)))
