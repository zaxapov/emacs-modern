;;; NASM coding mode
(use-package nasm-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.[n]*\\(asm\\|s\\)\\'" . nasm-mode)))

;;; Markdown setup
(use-package markdown-mode)
(use-package markdown-toc)

;;; Reverse input for russian language
(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  :config
  (reverse-im-mode t))
