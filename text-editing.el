;;; COMPLETION
(use-package company
  :config
  (setq company-selection-wrap-around t) ;; wrap around to the beginnning
  ;; (company-tng-mode t) ;; use TAB key to go through completions
  (setq company-minimum-prefix-length 2) ;; start competion only after 2 symbols
  (setq company-idle-delay 0.1) ;; completion delay time
  (global-company-mode 1))
;;; COMPANY MODULES
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
  (setq company-quickhelp-delay 2) ;; documentation delay in seconds
  (company-quickhelp-mode t))
(use-package company-statistics
  :config
  (require 'company-statistics)
  (add-hook 'after-init-hook 'company-statistics-mode))

;;; REMEMBER THE PLACE WHERE THE CURSOR WAS LAST TIME
(save-place-mode 1)

;;; CODE FOLDING ON/OFF
;;; SHOW AVAILABLE COMMANDS WITH 'M-x hs-'
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode 1)))

;;; INDENTATION
(setq-default indent-tabs-mode nil  ;; use spaces, not tabs
              tab-width 4)          ;; indentation width
;;; DELETE BY INDENT WIDTH, NOT BY SINGLE SPACES
;; (setq backward-delete-char-untabify-method 'hungry)

;;; AUTO-CLOSE PARENS, BRACKETS AND QUOTES?
(electric-pair-mode 1)

;;; SENTENCES END WITH A SINGLE SPACE
(setq sentence-end-double-space nil)

;;; ENABLE UPCASE/DOWNCASE-REGION COMMANDS
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; PLACE OVER SELECTED REGION
(delete-selection-mode t)
