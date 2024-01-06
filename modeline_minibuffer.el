;;; COMMANDS (MINIBUFFER) COMPLETION
;;; ENABLE VERTICO
(use-package vertico
  :init
  (vertico-mode)
  ;;; DIFFERENT SCROLL MARGIN
  ;; (setq vertico-scroll-margin 0)
  ;;; SHOW MORE CANDIDATES
  (setq vertico-count 20)
  ;;; GROW AND SHRINK THE VERTICO MINIBUFFER
  (setq vertico-resize t)
  ;;; OPTIONALLY ENABLE CYCLING FOR `vertico-next' AND `vertico-previous'.
  (setq vertico-cycle t))
(use-package vertico-directory
  :straight nil
  :after vertico
  :ensure nil
  ;;; MORE CONVENIENT DIRECTORY NAVIGATION COMMANDS
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;;; TIDY SHADOWED FILE NAMES
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
;;; PERSIST HISTORY OVER EMACS RESTARTS. VERTICO SORTS BY HISTORY POSITION.
(use-package savehist
  :init
  (savehist-mode))

;;; RECURSIVE MINIBUFFERS ON/OFF
(setq enable-recursive-minibuffers t)

;;; SHOW DESCRIPTIONS FOR COMMANDS IN MINIBUFFER
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;;; MODELINE
(setq mode-line-compact 1)
;;; SHOW THE BUFFER SIZE IN THE MODELINE
(size-indication-mode 1)
;;; SHOW COLUMN NUMBER ALONG WITH LINE NUMBER IN MODELINE
(column-number-mode 1)
