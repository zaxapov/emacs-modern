;;; DEFAULT FRAME SIZE
(add-to-list 'default-frame-alist '(width . 90))
(add-to-list 'default-frame-alist '(height . 38))

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
(load-theme 'tron-legacy t)

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
