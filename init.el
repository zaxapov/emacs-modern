;;; Keybindings:
;;
;; "M-x"  Show all commands
;;        – hold down the "Meta key" and press <x>
;;        – the "Meta key" is usually <Alt> on Linux/Windows and <Option> on Mac
;;
;; "C-g"  Get out! Press <Ctrl>+<g> to cancel whatever happens – or hit 3x <ESC>
;;
;; "F12"  Toggle between dark and light theme
;;
;;; Examples:
;;
;; "M-x eon-"                     Show all commands defined by Emacs ONBOARD
;; "M-x eon-goto-user-init-file"  Visit main config file: .emacs or init.el
;; "M-x check-parens"             Check if all parens match in Emacs Lisp code
;; "M-x help"                     Reach the ultimate help menu
;;
;; "C-h o" Place the cursor behind a keyword, function, variable or other symbol
;;         to issue the command `describe-symbol' via keybinding
;;         and view the symbol's documentation
;;
;; "M-;"   Comment/uncomment a selected piece of text or code
;;
;;; Code:

;;  ____________________________________________________________________________
;;; GARBAGE COLLECTION
;; <https://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Garbage-Collection>

;; Temporarily set a high value of 256 MB to trigger less garbage collections
;; during initialization. The Emacs default is a threshold of 800 KB
(setq gc-cons-threshold (* 256 1000000))

;; Then lower the threshold to 16 MB during normal operation to prevent longer
;; GC pauses, but still have it at a higher value than the default to experience
;; less mini-interruptions – eg. while scrolling larger buffers.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1000000))))

;; Show a message when garbage collection happens? Useful while tuning the GC
(setq garbage-collection-messages nil)

;; Diagnostics
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s with %d garbage collections."
                     (format "%.3f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;  ____________________________________________________________________________
;;; PACKAGE MANAGEMENT

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

;; Installing packages

;; Keybinding descovery minor mode
;; -------------------------
(use-package which-key
  :config
  (which-key-mode t))

;; NASM coding mode
;; -----------------
(use-package nasm-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.[n]*\\(asm\\|s\\)\\'" . nasm-mode)))

;; MEOW mode - mode for better hotkeys
;; ------------------------------------
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("a" . meow-M-x)
   '("x" . "C-x")
   '("b a" . kill-buffer-and-window)
   '("b b" . switch-to-buffer)
   '("b e" . eval-buffer)
   '("b k" . kill-current-buffer)
   '("b s" . save-buffer)
   '("f f" . find-file)
   '("f v" . view-file)
   '("f r" . recentf)
   '(";" . comment-line))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
(use-package meow
  :config
  (require 'meow)
  (meow-setup)
  (meow-global-mode 1))

;; Markdown setup
;; ---------------
(use-package markdown-mode)
(use-package markdown-toc)

;; Buffer competion
;; -----------------
(use-package company
  :config
  (setq company-selection-wrap-around t) ;; wrap around to the beginnning
  ;; (company-tng-mode t) ;; use TAB key to go through completions
  (setq company-minimum-prefix-length 2) ;; start competion only after 2 symbols
  (setq company-idle-delay 0.1) ;; completion delay time
  (global-company-mode 1))
;; Company modules
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

;; Commands (minibuffer) completion
;; -----------------------------------
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))
(use-package vertico-directory
  :straight nil
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))
;; Show descriptions for commands in minibuffer
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; Magit (Git inside Emacs)
;; -------------------------
(use-package magit)

;; LATEX
;;---------------------------
;; AucTeX settings - almost no changes
(use-package tex
  :straight nil
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
         ("C-S-e" . latex-math-from-calc))
  :config
  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0) 
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad))))))))

(use-package preview
  :straight nil
  :after latex
  :hook ((LaTeX-mode . preview-larger-previews))
  :config
  (defun preview-larger-previews ()
    (setq preview-scale-function
          (lambda () (* 1.25
                   (funcall (preview-scale-from-face)))))))

;; CDLatex settings
(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map 
              ("<tab>" . cdlatex-tab)))

;; Yasnippet settings
(use-package yasnippet
  :ensure t
  :hook ((LaTeX-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
  (use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))

  (setq yas-triggers-in-field t)
  
  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))

;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas
;; fields
(use-package cdlatex
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
  :config
  (use-package yasnippet
    :bind (:map yas-keymap
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB" . yas-next-field-or-cdlatex))
    :config
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))

;; Array/tabular input with org-tables and cdlatex 
(use-package org-table
  :straight nil
  :after cdlatex
  :bind (:map orgtbl-mode-map
              ("<tab>" . lazytab-org-table-next-field-maybe)
              ("TAB" . lazytab-org-table-next-field-maybe))
  :init
  (add-hook 'cdlatex-tab-hook 'lazytab-cdlatex-or-orgtbl-next-field 90)
  ;; Tabular environments using cdlatex
  (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
                                       "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
                                       "\\begin{bmatrix} ? \\end{bmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
                                       "\\begin{pmatrix} ? \\end{pmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
                                        "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                                       lazytab-position-cursor-and-edit
                                       nil t nil))
  :config
  ;; Tab handling in org tables
  (defun lazytab-position-cursor-and-edit ()
    ;; (if (search-backward "\?" (- (point) 100) t)
    ;;     (delete-char 1))
    (cdlatex-position-cursor)
    (lazytab-orgtbl-edit))

  (defun lazytab-orgtbl-edit ()
    (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
    (orgtbl-mode 1)
    (open-line 1)
    (insert "\n|"))

  (defun lazytab-orgtbl-replace (_)
    (interactive "P")
    (unless (org-at-table-p) (user-error "Not at a table"))
    (let* ((table (org-table-to-lisp))
           params
           (replacement-table
            (if (texmathp)
                (lazytab-orgtbl-to-amsmath table params)
              (orgtbl-to-latex table params))))
      (kill-region (org-table-begin) (org-table-end))
      (open-line 1)
      (push-mark)
      (insert replacement-table)
      (align-regexp (region-beginning) (region-end) "\\([:space:]*\\)& ")
      (orgtbl-mode -1)
      (advice-remove 'orgtbl-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)))
  
  (defun lazytab-orgtbl-to-amsmath (table params)
    (orgtbl-to-generic
     table
     (org-combine-plists
      '(:splice t
                :lstart ""
                :lend " \\\\"
                :sep " & "
                :hline nil
                :llend "")
      params)))

  (defun lazytab-cdlatex-or-orgtbl-next-field ()
    (when (and (bound-and-true-p orgtbl-mode)
               (org-table-p)
               (looking-at "[[:space:]]*\\(?:|\\|$\\)")
               (let ((s (thing-at-point 'sexp)))
                 (not (and s (assoc s cdlatex-command-alist-comb)))))
      (call-interactively #'org-table-next-field)
      t))

  (defun lazytab-org-table-next-field-maybe ()
    (interactive)
    (if (bound-and-true-p cdlatex-mode)
        (cdlatex-tab)
      (org-table-next-field))))
;; Themes
;; -------
(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes") 
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))
(use-package spacemacs-theme)
(use-package solarized-theme)
(use-package dracula-theme)
(setq auto-dark-light-theme 'spacemacs-light)
(setq auto-dark-dark-theme 'dracula)
(use-package auto-dark
  :config (auto-dark-mode t))

;; ;; Better modeline
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))
;;  ____________________________________________________________________________
;;; SYSTEM

;; Prevent stale elisp bytecode from shadowing more up-to-date source files
(setq load-prefer-newer t)

;; Use short y-or-n answers instead of long yes-or-no
(if (>= emacs-major-version 28)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

;; Smooth scrolling
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1) ;; Pixel-based scrolling
  ;; Enable next option only on Linux, Windows and some Emacs ports for macOS
  ;; (pixel-scroll-precision-use-momentum 1) ;; Emacs to continue to “drift” the display after it stops
  (setq pixel-scroll-precision-large-scroll-height 40.0)) ;; emulate smooth scrolling with ordinary mouse

;; Fullscrenn keybinding
(global-set-key (kbd "C-s-f") #'toggle-frame-fullscreen)

;; Eval buffer keybinding
(global-set-key (kbd "C-c e") #'eval-buffer)

;; Horizontal scroll bar
(horizontal-scroll-bar-mode t)

;; Horizontall scrolling with mouse (reversed for Mac OS)
;; (global-set-key [wheel-right] '(lambda ()
;;                                      (interactive)
;;                                      (scroll-left 2)))
;; (global-set-key [wheel-left] '(lambda ()
;;                                      (interactive)
;;                                      (scroll-right 2)))

;; Right click context menu
(when (>= emacs-major-version 28)
  (context-menu-mode 1))

;;  ____________________________________________________________________________
;;; FONTS
;;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Fonts>

;; Set default font
(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t)))
 ((eq system-type 'darwin) ;; macOS
  (when (member "Iosevka Custom" (font-family-list))
    (set-frame-font "Iosevka Custom-18" t t)))
 ((eq system-type 'gnu/linux)
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono" t t))))

;;  ____________________________________________________________________________
;;; INITIAL FRAME

;; … or set the default width of the Emacs frame in characters
(add-to-list 'default-frame-alist '(width . 90))

;; … and set the default height of the Emacs frame in lines
(add-to-list 'default-frame-alist '(height . 38))

;;  ____________________________________________________________________________
;;; CURSOR
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Cursor-Display>

;; To learn about available cursors, place your cursor behind 'cursor-type'
;; in the code below or do "M-x describe-symbol RET cursor-type RET"

;; Set the cursor type
;; Comment out the following expression to change the curser into to a bar
(add-to-list 'default-frame-alist '(cursor-type . bar))

;; Turn on/off cursor blinking by default?
(blink-cursor-mode -1)  ; 1 means 'on' / -1 means 'off'

;; Cursor blinking interval in seconds
(setq blink-cursor-interval 0.4)

;; Emphasize the cursor when running Emacs in a text terminal?
(setq visible-cursor nil)

;; Make sure to highlight the current line only in the active window.
(setq hl-line-sticky-flag nil)

;;  ____________________________________________________________________________
;;; USER INTERFACE

;; Menu bar: on/off by default?
(menu-bar-mode 1)
(global-set-key (kbd "M-`") #'menu-bar-mode)

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

;;  ____________________________________________________________________________
;;; MODELINE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Mode-Line>

;; Compress the mode line? If non-nil, repeating spaces are compressed into
;; a single space. If 'long', this is only done when the mode line is longer
;; than the current window width (in columns).
(setq mode-line-compact nil)

;; Show the buffer size in the modeline
(size-indication-mode 1)

;; Show column number along with line number in modeline
(column-number-mode 1)

;;  ____________________________________________________________________________
;;; ELDOC
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Lisp-Doc>

(setq eldoc-minor-mode-string ""
      eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
      eldoc-echo-area-display-truncation-message nil
      eldoc-echo-area-prefer-doc-buffer t
      eldoc-echo-area-use-multiline-p t)

;;  ____________________________________________________________________________
;;; PINENTRY

(require 'epg-config)
(setq epg-pinentry-mode 'loopback)

;;  ____________________________________________________________________________
;;; WINDOW MANAGEMENT
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Windows>
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Window-Convenience>

;; Display-buffer: avoid resizing
(setq even-window-sizes nil)

;; Focus follows mouse?
(setq mouse-autoselect-window nil
      focus-follows-mouse nil)

;; Default window navigation – simply switch to the next window in order
;; Added for convenience; the default keybinding is "C-x o"
(global-set-key (kbd "M-o") #'other-window)

;; Navigate windows by direction instead
;; (require 'windmove)
;; (setq windmove-wrap-around nil)
;; (global-set-key (kbd "s-j") #'windmove-down)
;; (global-set-key (kbd "s-k") #'windmove-up)
;; (global-set-key (kbd "s-h") #'windmove-left)
;; (global-set-key (kbd "s-l") #'windmove-right)

;; Undo/redo window layouts
(require 'winner)
(winner-mode 1)
(define-key winner-mode-map (kbd "C-x 4 u") #'winner-undo)
(define-key winner-mode-map (kbd "C-x 4 r") #'winner-redo)

;;  ____________________________________________________________________________
;;; BUFFERS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Buffers>

(when (version<= "27.1" emacs-version)
  (setq switch-to-buffer-obey-display-actions t))

;; Uniquify buffer names for buffers that would have identical names
(setq uniquify-buffer-name-style 'forward)

;; Kill the current buffer immediately instead of presenting a selection
;; It's the equivalent to "close tab" in a web browser or other editors
(global-set-key (kbd "C-x k") #'kill-buffer-and-window)

;; Define boring buffers globally, so they can be hidden
(defvar eon-boring-buffers '("\\` "
                             "\\`\\*Echo Area"
                             "\\`\\*Minibuf"
                             "\\`\\*Completions"
                             "\\`\\*Flymake log"
                             "\\`\\*Semantic SymRef"
                             "\\`\\*Backtrace"
                             "\\`\\*tramp"
                             ;; Some hidden buffers can be visited by ...
                             "\\`\\*scratch"        ; "C-z s s"
                             "\\`\\*Messages"       ; "C-h e"
                             "\\`\\*Bookmark List"  ; "C-x r l"
                             "\\`\\*Ibuffer"        ; "C-x C-b"
                             )
  "List of buffer names of buffers to hide on several occasions.
The elements of the list are regular expressions.")

;;  ____________________________________________________________________________
;;; IBUFFER – the buffer manager
;; <https://protesilaos.com/codelog/2020-04-02-emacs-intro-ibuffer/>

(require 'ibuf-ext)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)))

(setq ibuffer-marked-face 'dired-marked)

;; Don't show the boring buffers in Ibuffer
(setq ibuffer-never-show-predicates eon-boring-buffers)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Should the *scratch* buffer contain some initial content?
(setq initial-scratch-message "")

;;  ____________________________________________________________________________
;;; VISITING FILES AT POINT

;; "C-x C-v"       – Visit any resource under the cursor
;; "M-x ffap-menu" – Display a list of all ressources mentioned in this buffer

(global-set-key (kbd "C-x C-.") #'find-file-at-point)

;;  ____________________________________________________________________________
;;; BACKUP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Backup>

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

;;  ____________________________________________________________________________
;;; LOCKFILES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Interlocking>

;; Let Emacs keep track of files currently visited?
(setq create-lockfiles nil)

;;  ____________________________________________________________________________
;;; AUTO-SAVE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Auto-Save>
;; stop creating those #auto-save# files

(setq auto-save-default nil
      auto-save-interval 0)

;;  ____________________________________________________________________________
;;; HELP
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Help>

;; Show all options when running 'apropos' "C-h a" (fulltext search)
(require 'apropos)
(setq apropos-do-all t)

;;  ____________________________________________________________________________
;;; RECENT FILES

(require 'recentf)

;; Turn on recent file mode to visit recently edited files
(recentf-mode 1)

(setq recentf-max-menu-items 10
      recentf-max-saved-items 10)

;; Ignore some recently visited files, eg. to prevent them from showing up
;; amongst recent files after package upgrades
(add-to-list 'recentf-exclude
             (expand-file-name (concat user-emacs-directory "elpa/")))

;; Use 'completing-read' to choose between recent files
(defun eon-find-recentf ()
  "Find recent file via completion in the minibuffer."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t) nil))
(global-set-key (kbd "C-x f") #'eon-find-recentf)

;;  ____________________________________________________________________________
;;; DIRED
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Dired>

(require 'dired)

;; The `dired' keybinding is "C-x d". This new keybinding is in accordance
;; with "C-x C-f" for visiting files
(global-set-key (kbd "C-x C-d") #'dired)

;; Switch to wdired-mode and edit directory content like a text buffer
(define-key dired-mode-map (kbd "e") #'dired-toggle-read-only)

;; Don't accumulate useless Dired buffers
(defun eon-dired-single-buffer (s)
  "When S is non-nil, prevent superfluous Dired buffers from accumulating.
Kills the current Dired buffer when entering a new directory"
  (when (not (null s))
    (cond
     ((version< emacs-version "28.1")
      (progn (put 'dired-find-alternate-file 'disabled nil)
             (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)
             (define-key dired-mode-map (kbd "^") (lambda ()
                                                    (interactive)
                                                    (find-alternate-file "..")))))
     (t (setq dired-kill-when-opening-new-dired-buffer t)))))

(eon-dired-single-buffer t)  ; set the default

;; Auto refresh dired when contents of a directory change
(require 'autorevert)
(setq auto-revert-verbose nil)
(add-hook 'dired-mode-hook #'auto-revert-mode)

;; Directory listings
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Hide details in file listings? Toggle via "S-("
            (dired-hide-details-mode 1)
            ;; Highlight current line?
            (hl-line-mode 1)))

;; Listing columns; Switch arguments with "C-u s"
;; Show all files: -DlhFA and hide backups with -B
(setq-default dired-listing-switches "-lhFA")

;; Copying files/directories
(setq dired-recursive-copies 'always)

;; Create directories if they don't exist
(setq dired-create-destination-dirs 'ask)

;; Mimic dual-pane file managers?
(setq dired-dwim-target t)

;; Images
(require 'image-dired)
(setq image-dired-thumb-margin 1
      image-dired-thumb-relief 0
      ;; Store thumbnails in the system-wide thumbnail location
      ;; e.g. ~/.local/cache/thumbnails to make them reusable by other programs
      image-dired-thumbnail-storage 'standard-large)

;;  ____________________________________________________________________________
;;; COMINT

(require 'comint)

(setq comint-input-ignoredups t
      comint-prompt-read-only t)

;;  ____________________________________________________________________________
;;; PROCED

;; Show and manage OS processes, like the command line programs top and htop

(require 'proced)

(setq proced-auto-update-interval 1)

(setq-default proced-auto-update-flag t
              proced-descend t)

;;  ____________________________________________________________________________
;;; NET-UTILS

(require 'net-utils)

(setq netstat-program "netstat"
      netstat-program-options '("-atupe"))

;;  ____________________________________________________________________________
;;; PRIMARY WEB BROWSER
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Hyperlinking>

;; This can be any graphical web browser, but also a built-in web browser

;; Set Emacs' `browse-url' function …

;; … to the system-wide default browser
(setq browse-url-browser-function #'browse-url-default-browser)

;; … to Firefox explicitly
;; (setq browse-url-browser-function #'browse-url-firefox)

;; … or to the Nyxt browser <https://nyxt.atlas.engineer/>
;; (setq browse-url-generic-program "nyxt")
;; (setq browse-url-browser-function #'browse-url-generic)

;; Keybinding
(global-set-key (kbd "C-x w w") #'browse-url)

;;  ____________________________________________________________________________
;;; CALENDAR
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Calendar_002fDiary>

(require 'calendar)

(setq calendar-date-style 'iso
      calendar-week-start-day 1
      calendar-weekend-days '(6 0))

;;  ____________________________________________________________________________
;;; GENERAL EDITING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Basic>

;; UTF-8
(prefer-coding-system 'utf-8)

;; Remember the place where the cursor was last time
(save-place-mode 1)

;; Set desired line length in characters
(setq-default fill-column 80)

;; Draw a ruler at fill-column length
(global-display-fill-column-indicator-mode 1)

;; While a text selection is active, typing characters replaces
;; the selection with the typed characters (default: -1 = off)
(delete-selection-mode 1)

;; move cursor by camelCase
(global-subword-mode 1)

;;  ____________________________________________________________________________
;;; LINE NUMBERS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Display-Custom>

;; Line numbers on or off? Toggle with "M-x display-line-numbers-mode" or
;; set it here for all programming modes. Goto line: "M-g M-g"
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))

;;  ____________________________________________________________________________
;;; LINE WRAPPING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Line-Truncation>

;; Truncate long lines in programming modes?
;; By default, lines are continued visually on the next screen-line
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Continuation-Lines>
;; For default behavior, do "M-x toggle-truncate-lines", or set the variable to nil
;; and restart Emacs to make it permanent.

;; turn off line wrapping
(set-default 'truncate-lines t)

;;  ____________________________________________________________________________
;;; FOLDING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Hideshow>

;; Code folding on or off? Show available commands: "M-x hs-"
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode 1)))

;;  ____________________________________________________________________________
;;; INDENTATION
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Indentation>

(setq-default indent-tabs-mode nil      ; don't use tabs but spaces
              tab-width 4)          ; set display width for tab characters

;; Delete the whole indentation instead spaces one-by-one via <backspace>?
;; (Possibly shadowed by 3rd-party packages like 'smartparens-mode'

(setq backward-delete-char-untabify-method 'hungry)

;;  ____________________________________________________________________________
;;; BRACKETS / PARENTHESIS
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Parentheses>

;; How to display matching parens generally?
(setq show-paren-style 'mixed
      show-paren-delay 0.0)

;; Auto-close parens, brackets and quotes?
(electric-pair-mode 1)

;;  ____________________________________________________________________________
;;; WHITESPACE
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Useless-Whitespace>

;; Indicate trailing whitespace in programming modes?
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; Cleanup trailing whitespace in programming modes
(define-key prog-mode-map (kbd "C-x c w") #'whitespace-cleanup)

;; Indicate trailing whitespace in "text" modes?
(add-hook 'text-mode-hook
          (lambda ()
            (setq show-trailing-whitespace 1)))
;; Cleanup trailing whitespace in "text" modes
(define-key text-mode-map (kbd "C-x c w") #'whitespace-cleanup)

;;  ____________________________________________________________________________
;;; SYNTAX CHECK / LINTER
;; <https://www.gnu.org/software/emacs/manual/html_mono/flymake.html>

(require 'flymake)

;; Disable the legacy backend
(remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)

;; Style the Flymake widget in the modeline
(setq flymake-mode-line-format
      '(" " "FlyM" flymake-mode-line-exception flymake-mode-line-counters))

;; Stop when first/last error is reached
(setq flymake-wrap-around nil)

(define-key flymake-mode-map (kbd "M-g E") #'flymake-show-project-diagnostics)
(define-key flymake-mode-map (kbd "M-g e") #'flymake-show-buffer-diagnostics)
(define-key flymake-mode-map (kbd "M-g n") #'flymake-goto-next-error)  ; default
(define-key flymake-mode-map (kbd "M-g p") #'flymake-goto-prev-error)  ; default

;;  ____________________________________________________________________________
;;; COMPILING
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Building>

;; Keep the compilation buffer in the background, except when there's an error
(add-to-list
 'display-buffer-alist
 '("\\*.*compilation\\*" (display-buffer-no-window)))

;;  ____________________________________________________________________________
;;; TEXT MODES / WRITING

;; Sentences end with a single space
(setq sentence-end-double-space nil)

;;  ____________________________________________________________________________
;;; LISP LANGUAGES
;; <https://www.gnu.org/software/emacs/manual/html_mono/emacs.html#Executing-Lisp>

(defvar eon-lisp-modes
  '( emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook
     lisp-mode-hook inferior-lisp-mode-hook
     scheme-mode-hook inferior-scheme-mode-hook
     eval-expression-minibuffer-setup))

;; Auto-close parentheses, brackets, quotes, etc.
(mapc (lambda (h) (add-hook h #'electric-pair-local-mode))
      eon-lisp-modes)

;; Highlight matching parens
(mapc (lambda (h)
        (add-hook h #'(lambda () (setq-local show-paren-style 'expression))))
      eon-lisp-modes)

(mapc (lambda (h) (add-hook h #'show-paren-local-mode))
      eon-lisp-modes)

;; Emacs Lisp is supported by Flymake, so let's use it
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(add-hook 'lisp-interaction-mode-hook (lambda () (flymake-mode -1)))

;; Emacs Lisp is supported by Semantic, so let's use this too
;; <https://www.gnu.org/software/emacs/manual/html_mono/semantic.html>
(add-hook 'emacs-lisp-mode-hook #'semantic-mode)

;; Emacs Lisp: don't truncate printed lists
(setq eval-expression-print-length nil)

;; Additional keybinding resembling other sexp-related keybindings
;; who usually begin with "C-M". Also useful editing non-lisp languages
(global-set-key (kbd "C-M-<backspace>") #'backward-kill-sexp)

;;  ____________________________________________________________________________
(provide 'onboard)
;;; onboard.el ends here

;; (defun cfg:reverse-input-method (input-method)
;;   "Build the reverse mapping of single letters from INPUT-METHOD."
;;   (interactive
;;    (list (read-input-method-name "Use input method (default current): ")))
;;   (if (and input-method (symbolp input-method))
;;       (setq input-method (symbol-name input-method)))
;;   (let ((current current-input-method)
;;         (modifiers '(nil (control) (meta) (control meta))))
;;     (when input-method
;;       (activate-input-method input-method))
;;     (when (and current-input-method quail-keyboard-layout)
;;       (dolist (map (cdr (quail-map)))
;;         (let* ((to (car map))
;;                (from (quail-get-translation
;;                       (cadr map) (char-to-string to) 1)))
;;           (when (and (characterp from) (characterp to))
;;             (dolist (mod modifiers)
;;               (define-key local-function-key-map
;;                 (vector (append mod (list from)))
;;                 (vector (append mod (list to)))))))))
;;     (when input-method
;;       (activate-input-method current))))

;; (cfg:reverse-input-method 'russian-computer)

;; ORG MODE
;; -----------------------------------------------------------------------------
(setq org-latex-preamble nil)
(setq org-latex-default-packages-alist nil)
(setq org-latex-with-hyperref nil)
(setq org-latex-packages-alist nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "257de69e8cc7ffaf40ed1ba4abbd8d4cb1db6526a3557a9526f321306b279355" default))
 '(package-selected-packages '(meow which-key spacemacs-theme nasm-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
