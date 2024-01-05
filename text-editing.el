;; COMPLETION
;; -----------------
(use-package company
  :config
  (setq company-selection-wrap-around t) ;; wrap around to the beginnning
  ;; (company-tng-mode t) ;; use TAB key to go through completions
  (setq company-minimum-prefix-length 2) ;; start competion only after 2 symbols
  (setq company-idle-delay 0.1) ;; completion delay time
  (global-company-mode 1))
;; COMPANY MODULES
(use-package company-emoji
  :config
  (require 'company-emoji)
  (add-to-list 'company-backends 'company-emoji))
(use-package company-math
    :config
    (require 'company-math)
    (add-to-list 'company-backends 'company-math-symbols-unicode))
(use-package pos-tip)
(use-package company-quickhelp ;; show documentation in completion window
  :config
  (setq company-quickhelp-delay 3) ;; documentation delay in seconds
  (company-quickhelp-mode t))
(use-package company-statistics
  :config
  (require 'company-statistics)
  (add-hook 'after-init-hook 'company-statistics-mode))

;; REMEMBER THE PLACE WHERE THE CURSOR WAS LAST TIME
(save-place-mode 1)

;;; LINE NUMBERS FOR PROG-MODE ONLY
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)))

;;; TURN ON/OFF LINE WRAPPING
;;; OFF
;; (set-default 'truncate-lines 1)
;;; ON
(global-visual-line-mode t)

;;; FOLDING
;; CODE FOLDING ON OR OFF? SHOW AVAILABLE COMMANDS: "M-x hs-"
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode 1)))

;;; INDENTATION
(setq-default indent-tabs-mode nil      ; don't use tabs but spaces
              tab-width 4)          ; set display width for tab characters
;;; DELETE BY TAB-WIDTH, NOT BY SINGLE SPACES
;; (setq backward-delete-char-untabify-method 'hungry)

;; ;; HOW TO DISPLAY MATCHING PARENS GENERALLY?
(setq show-paren-style 'expression
      show-paren-delay 0.0)
;; AUTO-CLOSE PARENS, BRACKETS AND QUOTES?
(electric-pair-mode 1)

;;; INDICATE TRAILING WHITESPACE IN PROGRAMMING MODES?
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace 1)))

;; CLEANUP TRAILING WHITESPACE IN PROGRAMMING MODES
(define-key prog-mode-map (kbd "C-x c w") #'whitespace-cleanup)

;; INDICATE TRAILING WHITESPACE IN "TEXT" MODES?
(add-hook 'text-mode-hook
          (lambda ()
            (setq show-trailing-whitespace 1)))
;; CLEANUP TRAILING WHITESPACE IN "TEXT" MODES
(define-key text-mode-map (kbd "C-x c w") #'whitespace-cleanup)

;; SENTENCES END WITH A SINGLE SPACE
(setq sentence-end-double-space nil)

;;; ENABLE UPCASE-REGION COMMAND
(put 'upcase-region 'disabled nil)

;;; PLACE OVER SELECTED REGION
(delete-selection-mode t)
